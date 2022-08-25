# Hydraulic conductivity (Log K) as a function of the repellency index (R).
## data loading ------------------------------------------------------------
### empty  workspace
rm(list=ls())

### check directory
getwd()
dir()

# setup
library(tidyverse)
library(broom)
library(svglite)
library(ggtext) # remotes::install_github("clauswilke/ggtext")

# Read file --------------------------------------------------------------
ks <- read.csv2("./DATA/RAW/SL_Kns.csv", encoding = "UTF-8", check.names = FALSE) %>%
  mutate(pr = factor(pr, levels = c("High PR", "Low PR"),
                     labels = c("High PR", "Low PR")),
         position = factor(position, levels = c("OWT", "WT"),
                           labels = c("OWT", "WT")),
         depth = factor(depth, levels = c("Topsoil", "Subsoil"),
                        labels = c("Topsoil", "Subsoil"))) %>%
  nest_by(pr, position, depth) %>%
  mutate(mod = list(lm(kns ~ tension, data = data))) %>%
  summarize(tidy(mod)) %>%
  dplyr::select(pr:estimate) %>%
  filter(term == "(Intercept)") %>% 
  dplyr::select(-term) %>%
  rename(Ks = estimate)

R_index <- read.csv2("./DATA/RAW/SL_soil_dataset.csv", encoding = "UTF-8", check.names = FALSE) %>%
  mutate(pr = factor(pr, levels = c("High PR", "Low PR"),
                     labels = c("High PR", "Low PR")),
         position = factor(position, levels = c("OWT", "WT"),
                           labels = c("OWT", "WT")),
         depth = factor(depth, levels = c("Topsoil", "Subsoil"),
                        labels = c("Topsoil", "Subsoil"))) %>% 
  dplyr::select(pr, position, depth, R)

data  <- merge(ks,R_index,by=c("pr", "position", "depth"))

# visualization ---------------------------------------------------------

label <-data %>%
  group_by(pr, position, depth) %>%
  summarize(mean_R = mean(R),
            mean_Ks = mean(Ks)) %>%
  group_by() %>%
  summarize(mod = tidy(lm(log10(mean_Ks) ~ mean_R, data = .))$estimate,
            r_sqr = glance(lm(log10(mean_Ks) ~ mean_R, data = .))$r.squared) %>%
  # label
  summarise(label= paste("<i>K<sub>sat</sub> =</i>", 
                         format(.$mod[2], digits=2),
                         "<i>R +</i>",
                         format(.$mod[1], digits=2),
                         "<i><br/>R^2 =</i>",
                         format(.$r_sqr[1], digits=2),
                         ""))

R_KS <- data %>%
  group_by(pr, position, depth) %>%
  summarize(mean_R = mean(R),
            mean_Ks = mean(Ks)) %>%
  ggplot(aes(x = mean_R, y = log(mean_Ks))) + 
  geom_point(aes(color = pr)) + 
  geom_smooth(method = lm, se = FALSE, color = "black", size = 0.5) +
  labs(x = "R index",
       y = "K<sub>sat</sub> [log cm min<sup> -1</sup>]") +
  scale_color_viridis_d(name = NULL,
                        begin = 0.3,
                        end = 0.8,
                        guide=guide_legend(
                          direction='horizontal',
                          title.position=NULL,
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
        text=element_text(family="TT Times New Roman", size=12),
        strip.background = element_rect(colour = NA),
        axis.title.y = element_markdown(),
        legend.position = 'bottom')+ 
  geom_richtext(data = as.data.frame(label),
                col = "black",
                size = 5,
                hjust = 1, 
                vjust = 1,
                aes(x = 2.05, y = -1.6, label = label),
                fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 15), "pt"))  # remove padding

R_KS

scale_factor <- 0.7
ggsave("./FIGURES/SL_R_KS.png", R_KS, width= 6*scale_factor, height = 5*scale_factor)
ggsave("./FIGURES/SL_R_KS.svg", R_KS, width= 6*scale_factor, height = 5*scale_factor)
         
