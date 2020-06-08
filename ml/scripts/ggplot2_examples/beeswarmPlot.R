# Load library for making beeswarm plots
library(ggbeeswarm)

md_speeding %>% 
  filter(vehicle_color == 'RED') %>%
  ggplot(aes(x = gender, y = speed)) + 
  # change point size to 0.5 and alpha to 0.8
  geom_beeswarm(cex = 0.5, alpha = 0.8) +
  # add a transparent boxplot on top of points
  geom_boxplot(alpha = 0)