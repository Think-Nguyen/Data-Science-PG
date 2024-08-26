pacman::p_load(tidyverse, tidymodels)
data("penguins", package = "palmerpenguins")

set.seed(2024)
penguin_split <- initial_split(penguins)
penguin_split

penguins_train <- training(penguin_split)
penguins_test <- testing(penguin_split)

penguin_CV <- vfold_cv(penguins_train)
penguin_CV

# Set up model
linear_model <- linear_reg() %>%
  set_engine("lm")

penguin_linear_workflow <- workflow() %>%
  add_model(linear_model) %>%
  add_formula(bill_length_mm ~ body_mass_g)

logistic_model <- logistic_reg() %>%
  set_engine("glm")

penguin_logistic_workflow <- workflow() %>%
  add_model(logistic_model) %>%
  add_formula(sex ~ body_mass_g)

# CV
penguin_linear_resamples <- fit_resamples(penguin_linear_workflow,
                                          resamples = penguin_CV) 
penguin_linear_resamples

penguin_logistic_resamples <- fit_resamples(penguin_logistic_workflow,
                                            resamples = penguin_CV,
                                            control = control_resamples(save_pred = TRUE))

# Metrics
penguin_linear_resamples %>% unnest(.metrics)
penguin_linear_resamples %>% collect_metrics()

penguin_logistic_resamples %>% unnest(.metrics)
penguin_logistic_resamples %>% collect_metrics()


penguin_linear_workflow %>%
  last_fit(penguin_split) %>%
  collect_metrics()

penguin_logistic_workflow %>%
  last_fit(penguin_split) %>%
  collect_metrics()




























