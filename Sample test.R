pacman::p_load(tidyverse, tidymodels, readr, skimr)
data <- readRDS('./sample_ex.rds')
data

data %>% ggplot(aes(Y)) +
  geom_histogram(bins = 20)

skim(data)

data_2 <- data %>%
  mutate(Y = log(Y))
data_2 %>% summary()
data %>% summary()

data_2 %>% select(X2) %>% min()

outliers <- sort(data_2$X2)[1:5]
data_2$X2[data$X2 %in% outliers] <- NA 
data_2 %>% summary()

data_2 <- data_2 %>%
  mutate(X3 = as.numeric(X3))
data_2 %>% summary()

data_2 %>% count(C1)
c1_values <- c('a', 'b', 'c', 'd')
data_2$C1[!data_2$C1 %in% c1_values] <- NA
data_2 <- data_2 %>% 
  mutate(C1 = as.factor(C1))
data_2

data_2 %>% count(C2)
c2_values <- c('W', 'X', 'Y', 'Z')
data_2$C2[!data_2$C2 %in% c2_values] <- NA
data_2 <- data_2 %>% 
  mutate(C2 = as.factor(C2))
data_2

is.na(data_2) %>% sum()
data_2 <- data_2 %>% na.omit()
data_2

set.seed(20242)
data_split <- initial_split(data_2, strata = Y)
data_train <- training(data_split)
data_test <- testing(data_split)

data_folds <- vfold_cv(data_train, v = 20, strata = Y)
data_folds

data_recipe <- recipe(Y ~ ., data = data_train) %>%
  step_BoxCox(X4) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ X1:X2) %>%
  step_dummy(all_nominal_predictors())
data_recipe %>% prep() %>% bake(new_data = NULL)

pacman::p_load(glmnet)
las_reg_model <- linear_reg(mixture = 1, penalty = tune()) %>%
  set_engine('glmnet')

wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(las_reg_model)

penalty_grid <- grid_regular(penalty(), levels = 100)
penalty_grid$penalty[98]

doParallel::registerDoParallel()
tune_res <- tune_grid(wf, 
                      resamples = data_folds, 
                      grid = penalty_grid)
tune_res %>% autoplot()

show_best(tune_res, metric = 'rmse')

best_params <- select_best(tune_res, metric = 'rmse')
best_params

final_wf <- finalize_workflow(wf, best_params)

final_fit <- last_fit(final_wf, data_split)
final_fit %>% collect_metrics()

final_model <- final_fit %>% extract_workflow()

new_data <- tibble(
  X1 = -1,
  X2 = 0.5,
  X3 = 2,
  X4 = 1,
  C1 = 'c',
  C2 = 'X'
)

predict(final_model, new_data = new_data)


















