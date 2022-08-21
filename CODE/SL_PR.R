# Penetration Resistance (PR) ---------------------------------------------
## data loading ------------------------------------------------------------

### empty  workspace
rm(list=ls())

### check directory
getwd()
dir()

# setup
library(stringr)
library(sf)
library(sp)
library(ggplot2)
library(gstat)
library(plotKML)
library(magrittr)
library(RColorBrewer)

# Read file --------------------------------------------------------------
PR_s1 <- read.csv2("./DATA/RAW/SL_PR.csv", encoding = "UTF-8") %>%
  dplyr::select(-POINT)

# change the degree "°" symbol to a space " "
PR_s1$COORD_X = gsub('°', ' ', PR_s1$COORD_X)
PR_s1$COORD_Y = gsub('°', ' ', PR_s1$COORD_Y)

#Replace "S" and "W" indicator for negative values
PR_s1$COORD_X <- (lapply(PR_s1$COORD_X, function(x){
  if (stringr::str_sub(x,-1,-1) == "W"){
    paste("-",stringr::str_sub(x,2,-2), sep = "")
  }
}))

PR_s1$COORD_Y <- (lapply(PR_s1$COORD_Y, function(x){
  if (stringr::str_sub(x,-1,-1) == "S"){
    paste("-",stringr::str_sub(x,1,-2), sep = "")
  }
}))

# convert from decimal minutes to decimal degrees
PR_s1$COORD_X = measurements::conv_unit(PR_s1$COORD_X, from = 'deg_dec_min', to = 'dec_deg')
PR_s1$COORD_Y = measurements::conv_unit(PR_s1$COORD_Y, from = 'deg_dec_min', to = 'dec_deg')

# calculate 0-5 cm and  5-10 cm values
PR_s1 <- PR_s1 %>%
  dplyr::mutate(d0_5 = (X1+X2+X3+X4+X5)/5,
                d5_10 = (X6+X7+X8+X9+X10)/5)%>%
  dplyr::select(COORD_X,
                COORD_Y,
                d0_5)

#Define projection
projection <- "+proj=longlat +datum=WGS84 +no_defs"
projection_UTM <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
PR_s1 <- sf::st_as_sf(PR_s1, coords = c("COORD_X", "COORD_Y"), crs = projection)
PR_s1 <- sf::st_transform(PR_s1, crs = projection_UTM)

#Load mask
SL_SHP <- sf::st_read("./DATA/RAW/sites_shapes/San Luis.shp", quiet = TRUE)

PR1_SL_sf <- sf::st_intersection(PR_s1, sf::st_transform(SL_SHP, projection_UTM))
PR1_SL_sp <- as_Spatial(PR1_SL_sf)

#Show a map
ggplot() + geom_sf(data = SL_SHP) + geom_sf(data = PR1_SL_sf) + labs(title = "San Luis") + coord_sf(datum = st_crs(32618))

# Data intePRolation ------------------------------------------------------

gridded(PR1_SL_sp) <- FALSE
# SL 0-5cm
v_sl_0 <- gstat::variogram(d0_5 ~ 1, loc = PR1_SL_sp)
plot(v_sl_0, plot.numbers=TRUE, xlab="separation distance",cutoff=4)

# # Choose lower SSerr
# SSerr <- c()
# vmf_sl_0 <- fit.variogram(v_sl_0, vgm(psill = 800000, model = "Sph", range = 50, nugget = 100000))
# SSerr <-c(SSerr, Sph = attr(vmf_sl_0, "SSErr"))
# vmf_sl_0 <- fit.variogram(v_sl_0, vgm(psill = 800000, model = "Gau", range = 50, nugget = 100000))
# SSerr <-c(SSerr, Gau = attr(vmf_sl_0, "SSErr"))
# vmf_sl_0 <- fit.variogram(v_sl_0, vgm(psill = 800000, model = "Mat", range = 50, nugget = 100000))
# SSerr <-c(SSerr, Mat = attr(vmf_sl_0, "SSErr"))
# vmf_sl_0 <- fit.variogram(v_sl_0, vgm(psill = 800000, model = "Exp", range = 50, nugget = 100000))
# SSerr <-c(SSerr, Exp = attr(vmf_sl_0, "SSErr"))
# min(SSerr)

# create variogram model
vmf_sl_0 <- fit.variogram(v_sl_0, vgm(psill = 800000, model = "Sph", range = 50, nugget = 100000))
plot(v_sl_0, plot.numbers=TRUE, xlab="separation distance", model=vmf_sl_0)

# Ordinary point kriging --------------------------------------------------
# create grid 
SL_grid <- plotKML::vect2rast(sf::as_Spatial(SL_SHP), cell.size=1)
plot(SL_grid)
PR_krig_sl_0 <- krige(formula = d0_5 ~ 1, locations = PR1_SL_sp, newdata = SL_grid,
                      model = vmf_sl_0)

# calculate standard deviation of PR and plot
PR_krig_sl_0$var1.sd <- sqrt(PR_krig_sl_0$var1.var)

# Reclassify kriged data with as 2 classes divided by median value
PR_krig_median <- median(PR_krig_sl_0$var1.pred, na.rm = TRUE)

PR_krig_sl_0$var1.class <- factor(PR_krig_sl_0$var1.pred > PR_krig_median,
                                   labels = c("Low PR", "High PR"))

# export interpolated map as shapefile ------------------------------------
PR_krig_sl_0 <- st_as_sf(as(PR_krig_sl_0, "SpatialPolygonsDataFrame"))
st_write(PR_krig_sl_0, "DATA/PROCESSED/RP_kriged_SL_0-5.shp", driver="ESRI Shapefile", delete_dsn=TRUE)