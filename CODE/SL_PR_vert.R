# Penetration Resistance (PR) ---------------------------------------------
## data loading ------------------------------------------------------------

### empty  workspace
rm(list=ls())

### check directory
getwd()
dir()

# setup
library(dplyr)
library(stringr)
library(measurements)
library(sf)
library(ggplot2)
library(tidyr)
library(svglite)

# Read file --------------------------------------------------------------
PR_s1 <- read.csv2("./DATA/RAW/SL_PR.csv", encoding = "UTF-8", check.names = FALSE) %>%
  dplyr::select(-POINT) %>%
  pivot_longer(cols = c('1':'81'), names_to = "depth", values_to  = "PR") %>%
  mutate(depth = as.integer(depth),
         PR = PR*1000) #kPa to Pa

summary(PR_s1)
# change the degree "°" symbol to a space " "
PR_s1$COORD_X = gsub('°', ' ', PR_s1$COORD_X)
PR_s1$COORD_Y = gsub('°', ' ', PR_s1$COORD_Y)

# Replace "S" and "W" indicator for negative values
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

# transform into spatial object 
projection <- "+proj=longlat +datum=WGS84 +no_defs"
projection_UTM <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
PR_s1 <- sf::st_as_sf(PR_s1, coords = c("COORD_X", "COORD_Y"), crs = projection)
PR_s1 <- sf::st_transform(PR_s1, crs = projection_UTM)

# classify sampling points into high and low PR classes ------------------

SL_class <- sf::st_read("./DATA/PROCESSED/PR_classes.shp", quiet = TRUE)

SL_high_PR <- SL_class %>%
  filter(var1_class == "High PR")
SL_low_PR <- SL_class %>%
  filter(var1_class == "Low PR")

PR_High_PR <- sf::st_intersection(PR_s1, SL_high_PR)
PR_Low_PR <- sf::st_intersection(PR_s1, SL_low_PR)
PR_sl_class <- rbind(PR_High_PR, PR_Low_PR)

# Visualization -----------------------------------------------------------
PR_vert <-PR_sl_class %>%
  group_by(depth, var1_class) %>%
  mutate(PR_mean = mean(PR, na.rm = TRUE)) %>%
  ggplot(aes(y = PR_mean, x = depth, color = var1_class)) +
  geom_smooth(aes(y = PR, x = depth, color = var1_class),
              size = 0.5,
              se = FALSE,
              ) +
  geom_line(size = 0.5,
            alpha = 0.5) +
  geom_point() + 
  coord_flip() +
  scale_x_continuous(trans = "reverse") +
  labs(x = "Depth [cm]",
       y = "Penetration resistance [Pa]") +
  scale_color_viridis_d(name = NULL,
                        begin = 0.3,
                        end = 0.8,
                        guide=guide_legend(
                              direction='vertical',
                              title.position='top',
                              title.hjust = .5,
                              title.vjust = .5,
                              label.hjust = .5,
                              label.position = 'right',
                              keywidth = 1,
                              keyheight = 1)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.spacing.y = unit(0, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        strip.background = element_rect(colour = NA),
        text=element_text(family="Times New Roman", size=12),
        axis.title.y = element_markdown(),
        legend.position = 'right')

PR_vert

scale_factor <- 0.7
ggsave("./FIGURES/SL_PR_vert.png", PR_vert, width= 8*scale_factor, height = 4*scale_factor)
ggsave("./FIGURES/SL_PR_vert.svg", PR_vert, width= 8*scale_factor, height = 4*scale_factor)

