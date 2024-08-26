library(tidyverse)
data(mpg, package = 'ggplot2')
mpg <- mpg %>% 
  select(cty, displ, drv)
mpg

mpg <- mpg %>%
  mutate(drv = factor(drv))

mpg %>% 
  ggplot(aes(cty)) +
  geom_histogram(col = 'black', fill = '#9f4fff')

skimr::skim_without_charts(mpg)

mpg %>%
  ggplot(aes(displ)) +
  geom_histogram()

mpg %>%
  ggplot(aes(drv, fill = drv)) +
  geom_bar(color = 'black') +
  labs(x = 'Drive',
       title = 'Bar Chart of Drive for the mpg Dataset.') +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

mpg %>% count(drv)

mpg %>%
  ggplot(aes(x = displ, y = cty)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Displacement (litres)",
       y = "City Fuel Efficiency (miles per gallon)"
  ) +
  ggtitle(
    "Scatterplot of City Fuel Efficiency Against Displacement\nfor the mpg Dataset"
  )

mpg %>%
  ggplot(aes(x = drv, y = cty, fill = drv)) +
  geom_boxplot() +
  labs(x = "Drive",
       y = "City Fuel Efficiency (miles per gallon)"
  )

cty_ <- mpg %>% select(cty)
cty_ %>% 
  mutate(cty = as.double(cty)) %>%
  summary()
