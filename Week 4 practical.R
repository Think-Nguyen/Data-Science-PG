library(tidymodels)
data(mpg, package = 'ggplot2')
mpg

mpg_recipe <- recipe(cty ~ displ + drv, data = mpg)
mpg_recipe

mpg_recipe <- mpg_recipe %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
mpg_recipe

mpg_recipe %>% prep()
mpg_recipe %>% prep() %>% tidy()
mpg_recipe %>% prep() %>% tidy(n = 1)

mpg_recipe <- mpg_recipe %>%
  step_dummy(all_nominal_predictors())
mpg_recipe

mpg_recipe %>% prep() %>% tidy()
mpg_recipe %>% prep() %>% tidy(n = 3)

mpg_recipe <- mpg_recipe %>%
  step_interact(terms = ~starts_with('drv'):displ)
mpg_recipe
mpg_recipe %>% prep() %>% tidy(n = 4)

mpg_wf <- workflow()
mpg_wf <- mpg_wf %>%
  add_recipe(mpg_recipe)
mpg_wf

mpg_recipe %>%
  prep() %>%
  bake(new_data = NULL)

new_data <- tibble(displ = c(1,2),
                   drv = c('f', 'r'))
new_data

new_data2 <- mpg_recipe %>%
  prep() %>%
  bake(new_data = new_data)

# Calculate mean and standard deviation of displ in new_data
displ_mean <- mean(new_data$displ)
displ_sd <- sd(new_data$displ)

# Center and scale the displ value for the first row
centered_scaled_displ <- (new_data$displ[1] - displ_mean) / displ_sd
round(centered_scaled_displ, 2)












