pacman::p_load(tidyverse, tidymodels, ISLR, janitor, glmnet)
data("Hitters", package = "ISLR")
hitters <- as_tibble(Hitters)
hitters

help("Hitters")

hitters <- hitters %>% 
  janitor::clean_names() %>%
  select(where(is.numeric))

hitters_recipe <- recipe(salary ~ ., data = hitters) %>%
  step_naomit(salary)
hitters_recipe %>% prep() %>% bake(new_data = NULL)

hitters %>% colnames()

hitters_recipe %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())
hitters_recipe
hitters_recipe %>% prep() %>% bake(new_data = NULL)

# lambda = 1
hitters_M1 <- linear_reg(mixture = 0, penalty = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

hitters_WF1 <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_M1)

hitters_ridge_fit1 <- hitters_WF1 %>%
  fit(hitters)

hitters_ridge_fit1 %>% tidy()

# lambda = 1000
hitters_M2 <- linear_reg(mixture = 0, penalty = 1000) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

hitters_WF2 <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_M2)

hitters_ridge_fit2 <- hitters_WF2 %>%
  fit(hitters)

hitters_ridge_fit2 %>% tidy()

get_coef <- function(penalties, data, recipe){
  coef <- list()
  model <-
    linear_reg(mixture = 0, penalty = i) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  WF <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model)
  for(i in penalties){
    coef[[i]] <- WF %>% fit(data) %>% tidy()
  }
  coef <- bind_rows(coef)
  return(coef)
}

hitters_coefs <- get_coef(c(1, 10, 50, 100, 250, 500, 1000), hitters, hitters_recipe)
hitters_coefs %>% count(penalty)





























