pacman::p_load(tidyverse, tidymodels, discrim, palmerpenguins)
data("penguins", package = "palmerpenguins")
penguins

penguin_recipe <- recipe(species ~ bill_length_mm + bill_depth_mm,
                         data = penguins) %>%
  step_impute_mean(all_predictors())
penguin_recipe %>% prep() %>% tidy()
penguin_recipe %>% prep() %>% tidy(n = 1) -> a
a[2,2]

penguin_model <- discrim_linear() %>%
  set_mode('classification')
penguin_model

penguin_wf <- workflow() %>%
  add_recipe(penguin_recipe) %>%
  add_model(penguin_model)
penguin_wf

penguin_fit <- penguin_wf %>%
  fit(penguins)
penguin_fit

new_data <- crossing(
  bill_length_mm = seq(
    from = min(penguins$bill_length_mm, na.rm = TRUE), 
    to = max(penguins$bill_length_mm, na.rm = TRUE),
    length = 500
  ),
  bill_depth_mm = seq(
    from = min(penguins$bill_depth_mm, na.rm = TRUE), 
    to = max(penguins$bill_depth_mm, na.rm = TRUE),
    length = 500
  )
)

new_data %>% 
  add_column(
    predict(penguin_fit, new_data = new_data)
  ) %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, fill = .pred_class)) + 
  geom_raster() + 
  labs(
    fill = "Species", 
    x = "Bill length (mm)", 
    y = "Bill depth (mm)",
    title = 'LDA'
  ) + 
  theme_bw() + 
  viridis::scale_fill_viridis(option="D", discrete=TRUE)

penguin_model2 <- discrim_quad() %>%
  set_mode('classification')
penguin_model2

penguin_wf2 <- workflow() %>%
  add_recipe(penguin_recipe) %>%
  add_model(penguin_model2)
penguin_wf2

penguin_fit2 <- penguin_wf2 %>%
  fit(penguins)
penguin_fit2

new_data %>% 
  add_column(
    predict(penguin_fit2, new_data = new_data)
  ) %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, fill = .pred_class)) + 
  geom_raster() + 
  labs(
    fill = "Species", 
    x = "Bill length (mm)", 
    y = "Bill depth (mm)",
    title = 'QDA'
  ) + 
  theme_bw() + 
  viridis::scale_fill_viridis(option="D", discrete=TRUE)
