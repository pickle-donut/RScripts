library(ggridges)

md_speeding %>% 
  mutate(day_of_week = factor(day_of_week, levels = c("Mon","Tues","Wed","Thu","Fri","Sat","Sun") )) %>% 
  ggplot(aes( x = percentage_over_limit, y = day_of_week)) + 
  # Set bandwidth to 3.5
  geom_density_ridges(bandwidth = 3.5) +
  # add limits of 0 to 150 to x-scale
  scale_x_continuous(limits = c(0,150)) +
  # provide subtitle with bandwidth
  labs(subtitle = 'Gaussian kernel SD = 3.5')



md_speeding %>% 
  mutate(day_of_week = factor(day_of_week, levels = c("Mon","Tues","Wed","Thu","Fri","Sat","Sun") )) %>% 
  ggplot(aes( x = percentage_over_limit, y = day_of_week)) + 
  # make ridgeline densities a bit see-through with alpha = 0.7
  geom_density_ridges(bandwidth = 3.5, alpha = 0.7) +
  # set expand values to c(0,0)
  scale_x_continuous(limits = c(0,150), expand = c(0,0)) +
  labs(subtitle = 'Guassian kernel SD = 3.5') +
  # remove y axis ticks
  theme(axis.ticks.y = element_blank())