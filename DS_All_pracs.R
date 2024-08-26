# Prac 1: Workflow
pacman::p_load(tidyverse, tidymodels, vip)
data(mpg, package = 'ggplot2')
mpg

## Data cleaning
mpg <- mpg %>%
  select(cty, displ, drv)
mpg

mpg <- mpg %>%
  mutate(
    drv = factor(drv)
  )
mpg

## Explore the data
skimr::skim_without_charts(mpg)

#### Visualize
mpg %>% ggplot(aes(cty)) +
  geom_histogram(colour = 'black', fill = 'orange')

## Split data
set.seed(20242)
mpg_split <- initial_split(mpg, strata = cty)
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)
mpg_split

## Pre-processing using recipe
mpg_recipe <- recipe(cty ~ ., data = mpg_train) %>%
  step_dummy(all_nominal_predictors())
mpg_recipe %>% prep() %>% bake(new_data = NULL)
unique(mpg$drv)

## Set up the model
mpg_model <- linear_reg() %>%
  set_mode('regression') %>%
  set_engine('lm')

## Set up the workflow
mpg_wf <- workflow() %>%
  add_recipe(mpg_recipe) %>%
  add_model(mpg_model)
mpg_wf

## Fit the model
mpg_fit <- mpg_wf %>% fit(mpg_train)
mpg_fit

## Get model coefficients using tidy()
mpg_fit %>% tidy()

## Get information about the model with glance()
mpg_fit %>% glance()

## Assumption Checking: extract model
mpg_lm <- mpg_fit %>% extract_fit_parsnip() %>% pluck('fit')
mpg_lm %>% summary()

## assumption checking plots
plot(mpg_lm) # which = 1, 3, 2
gglm::gglm(mpg_lm)

## Prediction
new_data <- tibble(
  displ = 3,
  drv = '4'
)
predict(mpg_fit, new_data = new_data, type = 'pred_int')
predict(mpg_fit, new_data = new_data, type = 'conf_int')

new_data2 <- tibble(displ = 2, drv = 'f')
predict(mpg_fit, new_data = new_data2)

## Variable Importance Plots(VIP)
mpg_fit %>% extract_fit_parsnip() %>% vip()
mpg_fit %>% extract_fit_parsnip() %>% vi()

## Assess the model fit using the test data
metrics <- metric_set(rmse, rsq)
mpg_metrics <- mpg_test %>%
  add_column(
    predict(mpg_fit, mpg_test)
  ) %>%
  metrics(truth = cty, .pred)
mpg_metrics

# Prac 2: Data cleaning
rbc <- readxl::read_excel('rbc_clean.xlsx')
skimr::skim_without_charts(rbc)

rbc <- rbc %>% filter(fitness != 'NA')

unique(rbc$sex)
rbc <- rbc %>% 
  mutate(
    sex = factor(if_else(
      sex == 'M', 'male', 
      if_else(sex == 'F', 'female', sex)))
  )
unique(rbc$sex)

rbc <- rbc %>% mutate(
  fitness = as.numeric(fitness)
) %>% 
  filter(fitness >= 0 & fitness <= 100)

rbc <- rbc %>% filter(country != 4)
rbc

skimr::skim_without_charts(rbc)

rbc %>% ggplot(aes(RBC)) + 
  geom_histogram()

rbc <- rbc %>% filter(RBC > 0)
rbc <- rbc %>% select(sex, fitness, country, RBC)

rbc <- readxl::read_excel('rbc_clean.xlsx')
unique(rbc$country)
rbc <- rbc %>% 
  select(sex, fitness, country, RBC) %>%
  filter(country != 4) %>%
  mutate(
    sex = factor(
      case_when(
        sex == 'M' ~ 'male',
        sex == 'F' ~ 'female',
        TRUE ~ sex
      )
    ),
    fitness = as.numeric(fitness),
    country = factor(country)) %>%
  filter(fitness >= 0 & fitness <= 100) %>% 
  drop_na() %>%
  filter(RBC > 0)
rbc
skimr::skim_without_charts(rbc)
count(rbc, sex)
count(rbc, country)

# Prac 3: Exploratory Data Analysis
data(mpg, package = 'ggplot2')
mpg
mpg <- mpg %>% select(cty, displ, drv)
mpg
count(mpg, cty)
count(mpg, displ)

mpg <- mpg %>% mutate(drv = factor(drv))
mpg %>% ggplot(aes(x = cty)) + geom_histogram(col = 'black', fill = 'orange')
skimr::skim_without_charts(mpg)
mean(mpg$cty)
min(mpg$cty)
mpg %>% ggplot(aes(x = displ)) + geom_histogram(col = 'black', fill = 'orange')
mpg %>% ggplot(aes(x = drv)) + geom_bar(fill = 'dark blue') +
  theme_bw()
count(mpg, drv)
mpg %>% ggplot(aes(x = displ, y = cty)) + 
  geom_point() +
  geom_smooth()

mpg %>% ggplot(aes(x = drv, y = cty, fill = drv)) + 
  geom_boxplot()

# Prac 4: Recipes
data(mpg, package = 'ggplot2')
mpg

# Create a recipe
mpg_recipe <- recipe(cty ~ displ + drv, data = mpg)
mpg_recipe <- mpg_recipe %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
mpg_recipe %>% prep() %>% tidy(n = 1)
mpg_recipe <- mpg_recipe %>%
  step_dummy(all_nominal_predictors())
mpg_recipe
mpg_recipe %>% prep() %>% tidy(n = 3)
mpg_recipe <- mpg_recipe %>%
  step_interact(terms = ~starts_with('drv'):displ)
mpg_recipe
mpg_recipe %>% prep() %>% tidy(n = 4)

mpg_wf <- workflow() %>%
  add_recipe(mpg_recipe)
mpg_wf
mpg_recipe %>% prep() %>% bake(new_data = NULL)
new_data <- tibble(displ = c(1, 2),
                   drv = c('f', 'r'))
# This is centering and scaling the new data using the sample mean and sd
mpg_recipe %>% prep() %>% bake(new_data = new_data)

# This is to use the mean and sd of the new data to do the normalization
m <- mean(new_data$displ)
sd <- sd(new_data$displ)
(new_data[1, ]$displ - m) / sd

# Prac 5: Regression
pacman::p_load(tidyverse, tidymodels, ISLR, hanitor, glmnet)
data('Hitters', package = ISLR)
hitters <- as_tibble(Hitters)
hitters

# clean names
hitters <- hitters %>% janitor::clean_names()
hitters

# select numeric variables
hitters <- hitters %>% select(where(is.numeric))
hitters

# create a recipe
hitters_recipe <- recipe(salary ~ ., data = hitters) %>%
  step_naomit(salary)
hitters_recipe %>% prep() %>% bake(new_data = NULL)
# use is.na() to check if a cell is N.A.
hitters_recipe <- hitters_recipe %>%
  step_normalize(all_predictors())

hitters_recipe %>% prep() %>% bake(new_data = NULL)

# ridge regression with lambda being 1
hitters_M1 <- linear_reg(
  mixture = 0,
  penalty = 1
) %>% 
  set_mode('regression') %>%
  set_engine('glmnet')

hitters_wf1 <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_M1)

hitters_ridge_fit1 <- hitters_wf1 %>% fit(hitters)
hitters_ridge_fit1 %>% tidy()

hitters_M2 <- linear_reg(
  mixture = 0,
  penalty = 1000
) %>%
  set_mode('regression') %>%
  set_engine('glmnet')

hitters_wf2 <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_M2)

hitters_ridge_fit2 <- hitters_wf2 %>% fit(hitters)
hitters_ridge_fit2 %>% tidy()

# visualising the coefficients
get_coef <- function(penalties, data, recipe){
  coef <- list()
  model <- linear_reg(
    mixture = 0,
    penalty = i
  ) %>%
    set_mode('regression') %>%
    set_engine('glmnet')
  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model)
  for (i in penalties){
    coef[[i]] <- wf %>% fit(data) %>% tidy()
  }
  coef <- bind_rows(coef)
  return (coef)
}
coefs <- get_coef(c(1, 10, 50, 100, 250, 500), hitters, hitters_recipe)
coefs %>% filter(term != '(Intercept)') %>%
  ggplot(aes(x = penalty, y = estimate, col = term)) +
  geom_line()

# Prac 6: Classification
pacman::p_load(tidyverse, tidymodels, discrim, palmerpenguins)
data('penguins', package = 'palmerpenguins')
penguins

# Create a recipe 
penguins_recipe <- recipe(species ~ bill_length_mm + bill_depth_mm, 
                          data = penguins) %>%
  step_impute_mean(all_predictors())
penguins_recipe %>% prep() %>% tidy(n = 1)
mean(penguins$bill_depth_mm, na.rm = TRUE)

# Set up an LDA model
penguins_model <- discrim_linear() %>%
  set_mode('classification') %>%
  set_engine('MASS')

penguins_wf <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_model)
penguins_wf

penguins_fit <- penguins_wf %>% fit(penguins)
penguins_fit

# plot decision boundaries of LDA
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
    predict(penguins_fit, new_data = new_data)
  ) %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, fill = .pred_class)) + 
  geom_raster() + 
  labs(
    fill = "Species", 
    x = "Bill length (mm)", 
    y = "Bill depth (mm)"
  ) + 
  theme_bw() + 
  viridis::scale_fill_viridis(option="D", discrete=TRUE)

# QDA model for penguins dataset
penguins_QDA <- discrim_quad() %>%
  set_mode('classification') %>%
  set_engine('MASS')

penguins_QDA_wf <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_QDA)

penguins_QDA_fit <- penguins_QDA_wf %>% fit(penguins)
penguins_QDA_fit

new_data %>% 
  add_column(
    predict(penguins_QDA_fit, new_data = new_data)
  ) %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, fill = .pred_class)) + 
  geom_raster() + 
  labs(
    fill = "Species", 
    x = "Bill length (mm)", 
    y = "Bill depth (mm)"
  ) + 
  theme_bw() + 
  viridis::scale_fill_viridis(option="D", discrete=TRUE)

# Prac 7: Yardstick (Measurements)
pacman::p_load(tidyverse, tidymodels, yardstick)
data('penguins', package = 'palmerpenguins')
penguins <- penguins %>% na.omit()
# na.omit() is from base R, and doesn't allow you to set columns
# drop_na() is from tidyr, and allows you to set specific columns

# model1: simple linear regression
penguin_recipe1 <- recipe(
  flipper_length_mm ~ body_mass_g, 
  data = penguins
)
penguin_model1 <- linear_reg() %>%
  set_mode('regression') %>%
  set_engine('lm')
penguin_wf1 <- workflow() %>%
  add_recipe(penguin_recipe1) %>%
  add_model(penguin_model1)
penguin_fit1 <- penguin_wf1 %>%
  fit(penguins)
penguin_fit1

# model2: logistic regression
penguin_recipe2 <- recipe(
  sex ~ body_mass_g,
  data = penguins
)
penguin_model2 <- logistic_reg() %>%
  set_mode('classification') %>%
  set_engine('glm')
penguin_wf2 <- workflow() %>%
  add_recipe(penguin_recipe2) %>%
  add_model(penguin_model2)
penguin_fit2 <- penguin_wf2 %>% fit(penguins)
penguin_fit2

# Generate predictions
penguins_pred <- penguins %>% 
  bind_cols(
    predict(penguin_fit1, penguins),
    predict(penguin_fit2, penguins),
    predict(penguin_fit2, penguins, type = 'prob')
  ) %>%
  select(sex, flipper_length_mm, starts_with('.pred'))
penguins_pred

# Measuring model performance (metrics)
penguins_pred %>% rmse(
  truth = flipper_length_mm,
  .pred
)
penguins_pred %>% rsq(
  truth = flipper_length_mm,
  .pred
)

# Combining metrics
quantitative_metrics <- metric_set(rmse, rsq)
penguins_pred %>% quantitative_metrics(
  truth = flipper_length_mm,
  .pred
)

# confusion matrix for the hard classification
penguins_pred %>% conf_mat(
  truth = sex,
  .pred_class
)
penguins_pred %>% sens(
  truth = sex,
  .pred_class
)
penguins_pred %>% spec(
  truth = sex,
  .pred_class
)
penguins_pred %>% precision(
  truth = sex,
  .pred_class
)
penguins_pred %>% recall(
  truth = sex,
  .pred_class
)

# soft classifications
penguins_pred %>% roc_curve(
  truth = sex,
  .pred_female
  # event_level = 'first' # to set which is the event in your model
) %>% autoplot()

penguins_pred %>% roc_auc(
  truth = sex,
  .pred_female
)

# Prac 8: Cross-validation
pacman::p_load(tidyverse, tidymodels)
data('penguins', package = 'palmerpenguins')

## Split the data
set.seed(2024)
penguin_split <- initial_split(penguins) # use 'prop = 4/5' to set the proportion
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)
penguin_split

# Split training data into k folds
penguin_cv <- vfold_cv(penguin_train)
penguin_cv

# set up models
linear_model <- linear_reg() %>%
  set_engine('lm')
penguin_linear_wf <- workflow() %>%
  add_model(linear_model) %>%
  add_formula(bill_length_mm ~ body_mass_g) # when without a recipe

logistic_model <- logistic_reg() %>%
  set_engine('glm')
penguin_logistic_wf <- workflow() %>%
  add_model(logistic_model) %>%
  add_formula(sex ~ body_mass_g)

# CV folds
penguin_linear_resamples <- fit_resamples(
  penguin_linear_wf,
  resamples = penguin_cv,
  # to save the prediction for collect_predictions()
  control = control_resamples(save_pred = TRUE)
)
# unnest() returns all of the metrics for each fold
penguin_linear_resamples %>% unnest(.metrics)
# collect_metrics() returns the average value of each metric across all folds
penguin_linear_resamples %>% collect_metrics()

penguin_logistic_resamples <- fit_resamples(
  penguin_logistic_wf,
  resamples = penguin_cv,
  control = control_resamples(save_pred = TRUE)
)
penguin_logistic_resamples %>% unnest(.metrics)
penguin_logistic_resamples %>% collect_metrics()

# Plot the roc_curve for each fold
penguin_pred <- penguin_logistic_resamples %>% collect_predictions()
penguin_pred %>% group_by(id) %>%
  roc_curve(truth = sex,
            .pred_female) %>% autoplot()

# fit the test set
penguin_linear_wf %>% last_fit(penguin_split) %>%
  collect_metrics()

penguin_logistic_wf %>% last_fit(penguin_split) %>%
  collect_metrics()

# Prac 9: Tuning hyper-parameters
ikea <- read_rds('ikea.rds')
ikea

## Clean the data
ikea <- ikea %>%
  select(price, name, category, depth, height, width) %>%
  mutate(price = log10(price)) %>%
  mutate(across(where(is.character), factor))
ikea

## Split the data
set.seed(2024)
ikea_split <- initial_split(ikea, strata = price)
ikea_train <- training(ikea_split)
ikea_test <- testing(ikea_split)
ikea_split

## CV folds
ikea_cv <- vfold_cv(ikea_train, v = 15, strata = price)
ikea_cv

pacman::p_load(textrecipes)
# set up a recipe
ikea_recipe <- recipe(price ~ ., data = ikea_train) %>%
  step_other(name, category, threshold = 0.01) %>%
  step_impute_mean(depth, height, width)
ikea_recipe %>% prep() %>% bake(new_data = NULL)  

# set up a model
ikea_model <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500
) %>%
  set_mode('regression') %>%
  set_engine('ranger')

# combining recipe and model into workflow
ikea_wf <- workflow() %>%
  add_recipe(ikea_recipe) %>%
  add_model(ikea_model)
ikea_wf

# set up tuning grids
ikea_grid <- grid_regular(
  mtry(c(1, 5)),
  min_n(),
  levels = 5
)
ikea_grid

# fit the model
doParallel::registerDoParallel()
ikea_tune <-  tune_grid(
    ikea_wf,
    resamples = ikea_cv,
    grid = ikea_grid
  )
write_rds(ikea_tune, 'ikea_tune.rds')
ikea_tune_readin <- read_rds('ikea_tune.rds')
ikea_tune %>% autoplot()

show_best(ikea_tune, metric = 'rmse')

# finalize the model
ikea_wf <- ikea_wf %>% 
  finalize_workflow(select_best(ikea_tune, metric = 'rmse'))
ikea_wf

# Testing the final model
ikea_fit <- ikea_wf %>% last_fit(ikea_split)
ikea_fit %>% collect_metrics()

ikea_fit %>% collect_predictions() %>% 
  ggplot(aes(price, .pred)) +
  geom_point() + 
  geom_smooth(method = 'lm') + 
  geom_abline(intercept = 0, slope = 1)

# Prac 10: Interpretation
pacman::p_load(tidyverse, tidymodels, palmerpenguins)
data('penguins', package = 'palmerpenguins')
penguins

# Create a recipe
penguins_recipe <- recipe(
  sex ~ ., data = penguins
) %>%
  step_naomit(sex) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

penguins_recipe %>% prep() %>% bake(new_data = NULL)
penguins_recipe %>% prep() %>% tidy(n = 2)
penguins_recipe %>% prep() %>% tidy(n = 3)

# set up the models
penguins_M1 <- logistic_reg() %>%
  set_mode('classification') %>%
  set_engine('glm')

penguins_M2 <- decision_tree(tree_depth = 4) %>%
  set_mode('classification') %>%
  set_engine('rpart')

penguins_wf1 <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_M1)

penguins_wf2 <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_M2)

penguins_fit1 <- penguins_wf1 %>% fit(penguins)
penguins_fit2 <- penguins_wf2 %>% fit(penguins)

penguins_fit1 %>% tidy()
penguins_fit2

# Variable Importance Plots
penguins_fit1 %>% extract_fit_parsnip() %>% vip()
penguins_fit2 %>% extract_fit_parsnip() %>% vip()

# Partial Dependency Plots
pacman::p_load(DALEXtra)
pred <- penguins %>% 
  select(-sex)
response <- as.integer(penguins$sex)
penguins_explain1 <- explain_tidymodels(
  penguins_fit1,
  data = pred,
  y = response,
  verbose = FALSE
)

penguins_profile1 <- model_profile(
  penguins_explain1,
  variables = 'bill_length_mm',
  N = NULL,
  groups = 'species'
)

plot(penguins_profile1)

penguins_explain2 <- explain_tidymodels(
  penguins_fit2,
  data = pred,
  y = response,
  verbose = FALSE
)

penguins_profile2 <- model_profile(
  penguins_explain2,
  variables = 'bill_depth_mm',
  N = NULL,
  groups = 'island'
)

plot(penguins_profile2)