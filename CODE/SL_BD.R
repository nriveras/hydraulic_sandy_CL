# Penetration Resistance (PR) ---------------------------------------------
## data loading ------------------------------------------------------------

### empty  workspace
rm(list=ls())

### check directory
getwd()
dir()

# setup
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
data <- read.csv2("./DATA/RAW/SL_soil_dataset.csv", encoding = "UTF-8", check.names = FALSE) %>%
  mutate(pr = factor(pr, levels = c("High PR", "Low PR"),
                       labels = c("High PR", "Low PR")),
         position = factor(position, levels = c("OWT", "WT"),
                           labels = c("OWT", "WT")),
         depth = factor(depth, levels = c("Topsoil", "Subsoil"),
                        labels = c("Topsoil", "Subsoil")))

# Descriptive statistics -------------------------------------------------
# standard error function
se <- function(x) sqrt(var(x) / length(x))

desc_est <- data %>%
  group_by(pr, position, depth) %>%
  summarise(bd_mean = mean(bd),
            bd_sd= sd(bd),
            bd_se = se(bd))

# set up model
# 1 factor
model_pr                <- glm(bd ~ pr, data = data, family = "Gamma")
model_position          <- glm(bd ~ position, data = data, family = "Gamma")
model_depth             <- glm(bd ~ depth, data = data, family = "Gamma")

# 2 factor interactions
model_pr_position       <- glm(bd ~ pr*position, data = data, family = "Gamma")
model_pr_depth          <- glm(bd ~ pr*depth, data = data, family = "Gamma")
model_position_depth    <- glm(bd ~ position*depth, data = data, family = "Gamma")

# 3 factor interactions
model_pr_position_depth <- glm(bd ~ pr*position*depth, data = data, family = "Gamma")

# Model significance
anova(model_pr, test = "F")
anova(model_position, test = "F")
anova(model_depth, test = "F")
anova(model_pr_position, test = "F")
anova(model_pr_depth, test = "F")
anova(model_position_depth, test = "F")
anova(model_pr_position_depth, test = "F")

# get (adjusted) weight means per group
model_means_pr <- emmeans(object = model_pr,
                          specs = ~pr,
                          quiet = TRUE)
model_means_position <- emmeans(object = model_position,
                                specs = ~position,
                                quiet = TRUE)
model_means_depth <- emmeans(object = model_depth,
                          specs = ~depth,
                          quiet = TRUE)

model_means_pr_position <- emmeans(object = model_pr_position,
                                specs = ~pr & position,
                                quiet = TRUE)
model_means_pr_depth <- emmeans(object = model_pr_depth,
                                   specs = ~pr & depth,
                                quiet = TRUE)
model_means_position_depth <- emmeans(object = model_position_depth,
                                   specs = ~position & depth,
                                   quiet = TRUE)

model_means_pr_position_depth <- emmeans(object = model_pr_position_depth,
                                   specs = ~pr & position & depth,
                                   quiet = TRUE)

   
# add letters to each mean: compact letter display (cld)
model_means_pr <- cld(object = model_means_pr,
                            adjust = "Tukey",
                            Letters = letters,
                            alpha = 0.05,
                            quiet = TRUE)
model_means_position <- cld(object = model_means_position,
                               adjust = "Tukey",
                               Letters = letters,
                               alpha = 0.05,
                               quiet = TRUE)
model_means_depth <- cld(object = model_means_depth,
                               adjust = "Tukey",
                               Letters = letters,
                               alpha = 0.05,
                               quiet = TRUE)

model_means_cld_pr_position <- cld(object = model_means_pr_position,
                                adjust = "Tukey",
                                Letters = letters,
                                alpha = 0.05,
                                quiet = TRUE)
model_means_cld_pr_depth <- cld(object = model_means_pr_depth,
                                adjust = "Tukey",
                                Letters = letters,
                                alpha = 0.05,
                                quiet = TRUE)
model_means_cld_position_depth <- cld(object = model_means_position_depth,
                                adjust = "Tukey",
                                Letters = letters,
                                alpha = 0.05,
                                quiet = TRUE)

model_means_cld_pr_position_depth <- cld(object = model_means_pr_position_depth,
                                adjust = "Tukey",
                                Letters = letters,
                                alpha = 0.05,
                                quiet = TRUE)

# show output
model_means_pr      
model_means_position
model_means_depth
model_means_cld_pr_position
model_means_cld_pr_depth
model_means_cld_position_depth
model_means_cld_pr_position_depth

# Visualization -----------------------------------------------------------
## bars  
plot_pr_position_depth <- desc_est %>%
  ggplot() +
  # y-axis
  scale_y_continuous(breaks = pretty_breaks(),
                     expand = expansion(mult = c(0,0.1))) +
  coord_cartesian(ylim = c(1, NA)) +
  labs(y = "Bulk dentisy [g cm<sup>3</sup>]",
       x = "") +
  # bars
  geom_bar(data = desc_est,
           aes(y = bd_mean, 
               x = position,
               fill = pr),
           stat = "identity", 
           color = "black") +
  # errorbars
  geom_errorbar(data = desc_est,
                aes(ymin = bd_mean - bd_se,
                    ymax = bd_mean + bd_se,
                    x = position),
                width = 0.1) +
  # letters
  # geom_text(data = model_means_cld_pr_position_depth,
  #           aes(y = emmean + SE,
  #               x = depth,
  #               label = str_trim(.group)),
  #           hjust = 0.5,
  #           vjust = -0.5) +
  facet_wrap(~pr*depth, nrow = 1) +
  # general layout
  theme_classic() +
  scale_fill_viridis_d(name = "",
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
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.spacing.y = unit(0, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        strip.background = element_rect(colour = NA),
        text=element_text(family="Times New Roman", size=12),
        axis.title.y = element_markdown())

plot_pr_position_depth

scale_factor <- 0.7
ggsave(file="FIGURES/DB.png", plot_pr_position_depth, 
width= 8*scale_factor, height = 4*scale_factor)
ggsave(file="FIGURES/DB.svg", plot_pr_position_depth, 
       width= 8*scale_factor, height = 4*scale_factor)

# Visualization -----------------------------------------------------------
## Point  
plot_pr_position_depth <- desc_est %>%
  ggplot() +
  # y-axis
  scale_y_continuous(breaks = pretty_breaks(),
                     expand = expansion(mult = c(0.1,0.1)) # add space over and under the figure
  ) +  
  coord_cartesian(ylim = c(NA, NA)) +
  labs(y = "Bulk dentisy [g cm<sup>3</sup>]",
       x = "") +
  # bars
  geom_point(data = desc_est,
             aes(y = bd_mean, 
                 x = position,
                 colour = pr),
             stat = "identity") +
  # errorbars
  geom_errorbar(data = desc_est,
                aes(ymin = bd_mean - bd_se,
                    ymax = bd_mean + bd_se,
                    colour = pr,
                    x = position),
                width = 0.2) +
  # letters
  # geom_text(data = model_means_cld_pr_position_depth,
  #           aes(y = emmean + SE,
  #               x = depth,
  #               label = str_trim(.group)),
  #           hjust = 0.5,
  #           vjust = -0.5) +
  facet_wrap(~pr*depth, nrow = 1) +
  # general layout
  theme_classic() +
  scale_color_viridis_d(name = "",
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
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.spacing.y = unit(0, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        strip.background = element_rect(colour = NA),
        text=element_text(family="Times New Roman", size=12),
        axis.title.y = element_markdown())

plot_pr_position_depth

scale_factor <- 0.7
ggsave(file="FIGURES/DB_point.png", plot_pr_position_depth, 
       width= 8*scale_factor, height = 4*scale_factor)
ggsave(file="FIGURES/DB_point.svg", plot_pr_position_depth, 
       width= 8*scale_factor, height = 4*scale_factor)


