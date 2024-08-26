pacman::p_load(tidyverse, tidymodels, vip)
data(mpg, package = 'ggplot2')
mpg

mpg <- mpg %>%
  select(cty, displ, drv)
mpg

mpg <- mpg %>% 
  mutate(
    drv = factor(drv)
)
mpg

skimr::skim_without_charts(mpg)

mpg %>% 
  ggplot(aes(cty)) +
  geom_histogram(col = 'black', fill = '#3fbf7f')

set.seed(20241)
mpg_spit <- initial_split(mpg, strata = cty)
mpg_train <- training(mpg_spit)
mpg_test <- testing(mpg_spit)
mpg_train

mpg_recipe <- recipe(cty ~ ., data = mpg_train) %>%
  step_dummy(all_nominal_predictors())
mpg_recipe %>% prep() %>% bake(new_data = NULL)

mpg_model <- linear_reg() %>%
  set_mode('regression') %>%
  set_engine('lm')

mpg_wf <- workflow() %>%
  add_model(mpg_model) %>%
  add_recipe(mpg_recipe)
mpg_wf

mpg_fit <- mpg_wf %>%
  fit(mpg_train)
mpg_fit

mpg_fit %>% tidy()
mpg_fit %>% glance()

mpg_lm <- mpg_fit %>%
  extract_fit_parsnip() %>%
  pluck('fit')

plot(mpg_lm)
gglm::gglm(mpg_lm)

new_data <- tibble(
  displ = 3,
  drv = '4'
)
predict(mpg_fit, new_data = new_data, type = 'pred_int')

new_data2 <- tibble(
  displ = 2,
  drv = 'f'
)
predict(mpg_fit, new_data = new_data2)

mpg_fit %>%
  extract_fit_parsnip() %>%
  vip()

mpg_fit %>%
  extract_fit_parsnip() %>%
  vi()

metrics <- metric_set(rmse, rsq)
metrics

mpg_metrics <- mpg_test %>%
  add_column(
    predict(mpg_fit, mpg_test)
  ) %>%
  metrics(
    truth = cty,
    estimate = .pred
  )
mpg_metrics























