pacman::p_load(tidyverse, tidymodels, palmerpenguins)
data("penguins", package = "palmerpenguins")
penguins

penguins_recipe <- recipe(sex ~ ., data = penguins) %>%
  step_naomit() %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())
penguins_recipe %>% prep() %>% bake(new_data = NULL) #%>% pull('year') %>% unique()

penguins %>% count(species)

penguins_M1 <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

penguins_M2 <- decision_tree(tree_depth = 4) %>%
  set_engine("rpart") %>%
  set_mode("classification")

penguins_WF1 <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_M1)

penguins_WF2 <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_M2)

penguins_fit1 <- penguins_WF1 %>% fit(penguins)
penguins_fit2 <- penguins_WF2 %>% fit(penguins)

penguins_fit1 %>% tidy()
penguins_fit2

pacman::p_load(vip)
penguins_fit1 %>% extract_fit_parsnip() %>% vip()
penguins_fit2 %>% extract_fit_parsnip() %>% vip()


# Partial Dependency Plots
pacman::p_load(DALEXtra)

pred <- penguins %>%
  select(-sex)
response <- as.integer(penguins$sex)

penguins_explain1 <- explain_tidymodels(penguins_fit1,
                                        data = pred,
                                        y = response,
                                        verbose = FALSE)
penguins_profile1 <- model_profile(penguins_explain1,
                                   variables = "bill_length_mm",
                                   N = NULL,
                                   groups = "species")
plot(penguins_profile1)


penguins_explain2 <- explain_tidymodels(penguins_fit2,
                                        data = pred,
                                        y = response,
                                        verbose = FALSE)
penguins_profile2 <- model_profile(penguins_explain2,
                                   variables = "bill_depth_mm",
                                   N = NULL,
                                   groups = "species")
plot(penguins_profile2)