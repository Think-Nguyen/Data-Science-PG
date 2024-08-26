pacman::p_load(tidyverse, tidymodels, glmnet, vip, discrim, caret, kernlab)
data <- readxl::read_excel('data.rds')
data <- readr::readRDS('data.rds')
data <- read_rds('data.rds')

# Data cleaning
data <- data %>% janitor::clean_names()
data <- data %>% select(col_1, col_2) (select)
data <- data %>% select(-col_3) (remove)
data <- data %>% select(where(is.numeric())
data <- data %>% mutate(across(where(is.character), factor))
data <- data %>% mutate(col_1 = factor(col_1))
data %>% count(col_1)
data$col_1[!data$col_1 %in% c(__)] <- NA
data <- data %>% 
  mutate(col_1 = case_when(col == 'A' ~ 1, TRUE ~ col_1))
data <- data %>%
  mutate(col_1 = case_when(col_1 >= x ~ col_1, col_1 < x ~ NA,
                           col_2 = case_when(col_2 %in% letters[1:4] ~ col_2, TRUE ~ NA))
data %>% filter(!between(col_1, x, y))
data %>% filter(col_1 < x)
data <- data %>% na.omit()
                                 
# EDA
skimr::skim(data)
data %>% ggplot(aes(col_1)) +
  geom_histogram(bins = x)
data %>% ggplot(aes(col_1, fill = col_1)) + 
  geom_bar(col = ‘black’)
data %>% ggplot(aes(x = col_1, y = col_2)) + 
  geom_point() + 
  geom_smooth()
                                 
# Assess the model fit
data_fit %>% predict(new_data)
data_pred <- data %>% 
  bind_cols(predict(data_fit, data) %>% select(_)
data_pred %>% 
  conf_mat(truth = col_1, estimate = .pred)
data_pred %>% 
  roc_curve(truth = col_1, estimate = .pred_TRUE) 	#(event_level = ‘second’) (roc_auc) %>% autoplot()
metrics <- metric_set(rmse, rsq, mae) (sens, spec, precision, recall)
data_metrics <- data_test %>% 
  add_column(predict(data_fit, data_test)) %>% 
  metrics(truth = col_1, estimate = .pred)
                                                                                      
# Assumption checking
data_lm <- data_fit %>% extract_fit_parsnip() %>% pluck('fit')
plot(data_lm) #(check assumptions)
                                                                                      
# VIP
data_fit %>% extract_fit_parsnip() %>% vip()   #vi()
                                                                                      
# Visualise LDA
new_data <- crossing(col_1 = seq(from = min(data$col_1, na.rm = TRUE), 
                                 to = max(data$col_1, na.rm = TRUE), 
                                 length = 500),
                     col_2 = seq(from = min(data$col_2, na.rm = TRUE), 
                                 to = max(data$col_2, na.rm = TRUE), 
                                 length = 500))
new_data %>% 
  add_column(data_fit, new_data = new_data) %>%
  ggplot(aes(x = col_1, y = col_2, fill = .pred_class)) +
  geom_raster() +
  theme_bw()
                                                                                                           
                                                                                                           
select_by_one_std_err(data_tune, metric = "rmse", penalty)
# (select the model whose performance is within one standard error of the best performing model)
select_by_pct_loss(data_tune, metric = "rmse", penalty, limit = 5)
# (select the model whose performance is within a certain percentage loss of the best performing model)

# Splitting datasets
set.seed()
data_split <- initial_split(data, strata = 'col_1')
data_train <- training(data_split)
data_test <- testing(data_split)


                                                                                      
