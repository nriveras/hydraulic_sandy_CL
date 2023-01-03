# Penetration Resistance (PR) visualization ------------------------------
# restart environment ----------------------------------------------------
rm(list=ls())

# ggmap setup -------------------------------------------------------------
#Get the latest Install
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

#Load the library
library(ggmap)
library(ggtext) # remotes::install_github("clauswilke/ggtext")
library(ggsn)

#Set your API Key
# Read key from external file
key <- paste(readLines("CODE/maps_static_API_key"))
ggmap::register_google(key = key)

#Notes: If you get still have a failure then I suggest to restart R and run the library and register google commands again.

# plot predicted maps -----------------------------------------------------
# from stackoverflow -------------------------------------------------------
# source: https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
library(ggplot2)
library(ggmap)
library(sf)
#> Linking to GEOS 3.6.2, GDAL 2.3.0, proj.4 5.1.0
## data loading ----------------------------------------------------------
SL_PR <- sf::st_read("DATA/PROCESSED/RP_kriged_SL_0-5.shp", quiet = TRUE)
sampling_sites <- sf::st_read("DATA/RAW/sites_shapes/sampling_sites.shp", quiet = TRUE)
st_crs(sampling_sites) <- st_crs("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +type=crs")

# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
SL_PR_3857 <- st_transform(SL_PR, 3857)
sampling_sites_3857 <- st_transform(sampling_sites, 3857)

map <- ggmap::get_map(location = c(lon = -71.421420, lat = -34.383415),
                      maptype = "hybrid", 
                      zoom = 17,
                      scale = 4,
                      source = "google",
                      crop=TRUE)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- sf::st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map_bbox <- ggmap_bbox(map)

limits <- st_bbox(SL_PR_3857)

map_vis <- ggmap(map_bbox) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = var1_class), 
          data = SL_PR_3857, 
          inherit.aes = FALSE, 
          color = "transparent", 
          alpha = 0.5,
          family="Times") +
  scale_fill_viridis_d(name = "Penetration Resistance",
                       begin = 0.3,
                       end = 0.8,
                       guide=guide_legend(
                         direction='horizontal',
                         title.position='top',
                         title.hjust = .5,
                         label.hjust = .5,
                         label.position = 'bottom',
                         keyheight = 0.7,
                         keywidth = 5,
                         inherit.aes = FALSE)) +
  # adding sampling sites
  geom_sf(aes(),
          data = sampling_sites_3857, 
          inherit.aes = FALSE,
          color = c("black"),
          show.legend = FALSE,
          shape = 0) +
  coord_sf(ylim = limits[c(2,4)] + c(-55,55),
           xlim = limits[c(1,3)] + c(-55,55)) +
  theme_void()+
  theme(title=element_text(face='bold'),
        legend.position = 'bottom') +
  # including scale bar
  scalebar(x.min = -7950760, x.max = -7950382,
           y.min = -4080540, y.max = -4080202,
           dist = 50, dist_unit = "m",
           st.bottom = TRUE, st.color = "white", 
           st.dist = .05,
           transform = FALSE)

map_vis
scale_factor <- 0.6

ggsave("./FIGURES/SL_PR.png", map_vis, width= 9*scale_factor, height = 8*scale_factor)

