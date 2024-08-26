pacman::p_load(tidyverse, tidymodels, yardstick)
data("penguins", package = "palmerpenguins")
penguins <- penguins %>% na.omit()
penguins

# Set up model
m1_model <- linear_reg() %>%
  set_mode('regression') %>%
  set_engine('lm')
m1_recipe <- recipe(flipper_length_mm ~ body_mass_g, data = penguins)

m1_wf <- workflow() %>%
  add_model(m1_model) %>%
  add_recipe(m1_recipe)

m2_model <- logistic_reg() %>%
  set_mode('classification') %>%
  set_engine('glm') 
m2_recipe <- recipe(sex ~ body_mass_g, data = penguins)

m2_wf <- workflow() %>%
  add_model(m2_model) %>%
  add_recipe(m2_recipe)

# Fit model
penguins_fit1 <- m1_wf %>%
  fit(penguins)
penguins_fit1

penguins_fit2 <- m2_wf %>%
  fit(penguins)
penguins_fit2

# Predict
penguins_pred <- penguins %>%
  bind_cols(predict(penguins_fit1, penguins),
            predict(penguins_fit2, penguins),
            predict(penguins_fit2, penguins, type = "prob")) %>%
  select(sex, flipper_length_mm,starts_with(".pred"))
penguins_pred

# Metrics
penguins_pred %>%
  rmse(truth = flipper_length_mm,
       estimate = .pred)

quantitative_metrics <- metric_set(rmse, mae, rsq)
penguins_pred %>%
  quantitative_metrics(truth = flipper_length_mm,
                       estimate = .pred)
penguins_pred %>%
  conf_mat(truth = sex,
           estimate = .pred_class)
penguins_pred %>%
  sens(truth = sex,
       estimate = .pred_class) 
penguins_pred %>%
  spec(truth = sex,
       estimate = .pred_class)
penguins_pred %>%
  precision(truth = sex,
            estimate = .pred_class)

penguins_pred %>%
  roc_curve(truth = sex, .pred_female) %>%
  autoplot()
penguins_pred %>%
  roc_auc(truth = sex, .pred_female)































