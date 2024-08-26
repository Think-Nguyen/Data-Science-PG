pacman::p_load(tidyverse, tidymodels)
ikea <- read_rds("ikea.rds")

# Clean data
ikea <- ikea %>%
  select(price, name, category, depth, height, width) %>%
  mutate(price = log10(price)) %>%
  mutate(across(where(is.character), factor))

# Split data
set.seed(2024)
ikea_split <- initial_split(ikea, strata = price)
ikea_train <- training(ikea_split)
ikea_test <- testing(ikea_split)

ikea_folds <- vfold_cv(ikea_train, v = 15,  strata = price)
ikea_folds

# Build model
pacman::p_load(textrecipes)
ikea_recipe <- recipe(price ~ ., data = ikea_train) %>%
  step_other(name, category, threshold = 0.01) %>%
  step_impute_mean(depth, height, width)
ikea_recipe %>% prep() %>% bake(new_data=NULL)

ikea_model <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 500) %>%
  set_mode("regression") %>%
  set_engine("ranger")

ikea_wf <- workflow() %>%
  add_recipe(ikea_recipe) %>%
  add_model(ikea_model)
ikea_wf

# Model tuning
ikea_grid <- grid_regular(mtry(c(1,5)),
                          min_n(),
                          levels = 5)
ikea_grid <- grid_regular(mtry(c(1,5)),
                          min_n(), levels = 5)
ikea_grid

doParallel::registerDoParallel()
ikea_tune <- tune_grid(ikea_wf,
                       resamples = ikea_folds,
                       grid = ikea_grid)
write_rds(ikea_tune,"ikea_tune.rds")

# Choosing model
ikea_tune %>% autoplot()

show_best(ikea_tune, metric = "rmse")

ikea_wf <- ikea_wf %>%
  finalize_workflow(select_best(ikea_tune, metric = "rmse"))
ikea_wf

# Test final model
ikea_fit <- ikea_wf %>% last_fit(split = ikea_split)
ikea_fit %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>%
  pull(.estimate)

ikea_fit %>% collect_predictions() %>%
  ggplot(aes(price, .pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1)
