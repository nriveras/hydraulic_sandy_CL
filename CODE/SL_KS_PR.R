# Hydraulic conductivity (Log K) as a function of the repellency index (R).
## data loading ------------------------------------------------------------
### empty  workspace
rm(list=ls())

### check directory
getwd()
dir()

# Read file --------------------------------------------------------------
data <- read.csv2("./DATA/RAW/SL_soil_dataset.csv", encoding = "UTF-8", check.names = FALSE) %>%
  mutate(pr = factor(pr, levels = c("High PR", "Low PR"),
                     labels = c("High PR", "Low PR")),
         position = factor(position, levels = c("OWT", "WT"),
                           labels = c("OWT", "WT")),
         depth = factor(depth, levels = c("Topsoil", "Subsoil"),
                        labels = c("Topsoil", "Subsoil")),
         Ks = Ks/60) %>%
  rename(SOM = OM)
# visualization ---------------------------------------------------------

data %>%
  group_by(pr, position, depth) %>%
  summarize(mean_R = mean(R),
            mean_Ks = mean(Ks)) %>%
  ggplot(aes(x = mean_R, y = log(mean_Ks))) + 
  geom_point(aes(color = pr)) + 
  geom_smooth(method = lm)

         