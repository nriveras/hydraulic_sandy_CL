# Unsaturated hydaulic conductivity (Kns)-----------------------------------
## data loading ------------------------------------------------------------

### empty  workspace
rm(list=ls())

### check directory
getwd()
dir()

# setup
library(tidyverse)
library(broom)
library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)
library(stringr)
library(measurements)
library(scales)
library(ggplot2)
library(tidyr)
library(svglite)
library(ggtext) # remotes::install_github("clauswilke/ggtext")

# Read file --------------------------------------------------------------
data <- read.csv2("./DATA/RAW/SL_Kns.csv", encoding = "UTF-8", check.names = FALSE) %>%
  mutate(pr = factor(pr, levels = c("High PR", "Low PR"),
                       labels = c("High PR", "Low PR")),
         position = factor(position, levels = c("OWT", "WT"),
                           labels = c("OWT", "WT")),
         depth = factor(depth, levels = c("Topsoil", "Subsoil"),
                        labels = c("Topsoil", "Subsoil")),
         replicate = factor(replicate, levels = c("1", "2","3","4"),
                        labels = c("1", "2","3","4")))

data_summary <- data %>%
  group_by(pr, position, depth, tension) %>% 
  summarize(kns = mean(kns)) %>%
  mutate(position_depth = paste(position, depth, sep = ' - '))

# Descriptive statistics --------------------------------------------------
# Visualization -----------------------------------------------------------

labels <-  data.frame(pr = c("High PR", "Low PR"),
                      label = c("(A)", "(B)"))

Kns <- data_summary %>%
  ggplot(aes(x = tension, 
             y = kns, 
             shape = position_depth, 
             colour = position_depth,
             fill = position_depth,
             linetype = position
             )) + 
  geom_point(size = 3) +
  facet_wrap(~ pr, scales = "free") +
  geom_smooth(method = lm, show.legend  = FALSE,  se = FALSE) +
  labs(y = "K [cm min<sup> -1</sup>]",
       x = "Soil water pressure [hPa]") +
  scale_colour_manual(name = "",
                      labels = c("OWT - Topsoil",  "OWT - Subsoil", "WT - Topsoil",  "WT - Subsoil"),
                      values = c("#31688e","#90d743", "#31688e","#90d743"), 
                      guide=guide_legend(
                        direction='vertical',
                        title.position='top',
                        title.hjust = .5,
                        title.vjust = .5,
                        label.hjust = .5,
                        label.position = 'right',
                        keywidth = 1,
                        keyheight = 1)) +
  scale_shape_manual(name = "",
                     labels = c("OWT - Topsoil",  "OWT - Subsoil", "WT - Topsoil",  "WT - Subsoil"),
                     values = c(23, 24, 23, 24),
                     na.translate = TRUE, drop = TRUE) +
  scale_fill_manual(name = "",
                    labels = c("OWT - Topsoil",  "OWT - Subsoil", "WT - Topsoil",  "WT - Subsoil"),
                    values = c("#31688e","#90d743", "transparent", "transparent"),
                    na.translate = TRUE, drop = FALSE) +
  theme_minimal() + 
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.spacing.y = unit(0, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        strip.background = element_rect(colour = NA),
        text=element_text(family="Times New Roman", size=12),
        axis.title.y = element_markdown()) +
  geom_text(data = labels,
            aes(Inf, Inf, label = label), 
            col = "black",
            size = 5,
            hjust = 1.3, 
            vjust = 1.5,
            inherit.aes = FALSE)

Kns
scale_factor <- 0.7
ggsave(file="FIGURES/Kns.png", Kns,
       width= 8*scale_factor, height = 4*scale_factor)
ggsave(file="FIGURES/Kns.svg", Kns, 
       width= 8*scale_factor, height = 4*scale_factor)

