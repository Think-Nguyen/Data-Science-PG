# case_study 2: Data cleaning -- lyrics
pacman::p_load(tidyverse, tidymodels, here, glue, lubridate)
file_path <- here('eurovision-lyrics.json')
file_path
songs <- jsonlite::fromJSON(file_path)
songs[[1]]

parse_song <- function(song){
  country <- song$Country
  artist <-song$Artist
  title <- song$Song
  year <- song$Year
  lyrics <- song$Lyrics
  translation <- song$`Lyrics translation`
  place <- song$Pl
  tibble(
    country, artist, title, year, lyrics, translation, place
  )
}

parse_song(songs[[1]])

songs <- songs %>% map_df(parse_song, .id = "ID")
songs

songs %>% count(ID) %>% filter(n > 1)

songs <- songs %>%
  select(-ID)
songs

songs %>% count(artist) %>% arrange(-n)

songs %>% filter(str_detect(artist, "Jed"))

songs %>% filter(country == "Australia")

songs %>% count(title) %>% arrange(-n)

range(songs$year)

songs %>% count(year) %>% 
  ggplot(aes(year, n)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

songs %>% sample_n(5) %>% 
  pull(lyrics) %>% cat(sep = "\n\n\n")

songs %>% count(place)
length(unique(songs$year))

songs %>% count(year, place) %>% 
  filter(place == "1") %>% 
  arrange(-n)

songs <- songs %>% 
  mutate(
    year = parse_number(year),
    decade = year - year %% 10
  )

songs %>% ggplot(aes(year, decade)) + geom_point()

songs %>% count(translation) %>% arrange(-n)

songs <- songs %>%
  mutate(
    english_lyrics = ifelse(
      translation == "English",
      lyrics, translation
    )
  )

songs %>% view()

filename <- glue("{today()}-eurovision.rds")
filename
write_rds(
  songs,
  here(filename)
)

# case_study 3: Pre-processing -- credit
pacman::p_load(tidyverse, tidymodels, here, vip,
               janitor, visdat, naniar, corrplot, skimr)
data(credit_data, package = "modeldata")
credit_data <- as_tibble(credit_data)
credit_data <- credit_data %>% clean_names()

skim(credit_data)
credit_recipe <- recipe(status ~ ., data = credit_data)
credit_recipe
summary(credit_recipe)

credit_recipe <- credit_recipe %>% 
  step_center(all_numeric()) %>%
  step_scale(all_numeric())
credit_recipe

get_skewness <- function(x){
  x = x[!is.na(x)]
  x_bar = mean(x)
  n <- length(x)
  v <- sum((x - x_bar) ^ 2) / (n - 1)
  skewness <- sum((x - x_bar) ^ 3) / ((n - 1) * v ^ (3 / 2))
  return(skewness)
}
get_skewness(rnorm(100))
get_skewness(rexp(100))

credit_data %>%
  summarise(
    across(where(is.numeric), get_skewness)
  )

credit_recipe <- 
  credit_recipe %>%
  step_BoxCox(all_numeric())
credit_recipe

vis_miss(credit_data)
gg_miss_upset(credit_data)

credit_recipe <- 
  credit_recipe %>%
  step_impute_mode(all_nominal()) %>%
  step_impute_mean(all_numeric())
credit_recipe

credit_recipe <- 
  credit_recipe %>%
  step_nzv(all_predictors())
credit_recipe

credit_data %>%
  select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot()

credit_recipe <- 
  credit_recipe %>%
  step_corr(all_numeric())
credit_recipe

credit_recipe <- 
  credit_recipe %>%
  step_dummy(all_nominal(), -status)
credit_recipe

tidy(credit_recipe)
tidy(credit_recipe, n = 1)

credit_recipe <- 
  credit_recipe %>%
  prep(data = credit_data)
credit_recipe

tidy(credit_recipe)
tidy(credit_recipe, n = 1)

credit_recipe %>% juice()

credit_recipe %>% bake(credit_data)

# case_study 4: Cross-validation -- titanic
pacman::p_load(tidyverse, tidymodels, titanic, janitor, paletteer)
theme_set(theme_bw())

data("titanic_train", package = "titanic")
titanic <- tibble(titanic_train)
titanic

## clean names
titanic <- clean_names(titanic)
titanic

## convert to factors
titanic <- titanic %>% 
  mutate_all(factor)
titanic

## split data into training and testing
titanic_split <- initial_split(titanic, strata = survived)
titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)

titanic_train
titanic_test

## cross-validation
titanic_cv <- vfold_cv(titanic_train, v = 5, strata = survived)
titanic_cv

## recipe
titanic_recipe <- 
  recipe(survived ~ sex + pclass, data = titanic_train) %>%
  step_dummy(all_predictors()) %>%
  step_interact(terms = ~starts_with("sex"): ~starts_with("pclass"))

## prep and juice
titanic_recipe %>% prep() %>% juice()

## model specification
titanic_model <- logistic_reg() %>% set_engine("glm")

## set workflow
titanic_wf <- workflow() %>%
  add_recipe(titanic_recipe) %>%
  add_model(titanic_model)
titanic_wf

## fit data
titanic_fit <- fit(titanic_wf, titanic_train)
titanic_fit %>% tidy()

## cross validation
titanic_fits <- fit_resamples(
  titanic_wf,
  resamples = titanic_cv,
  control = control_resamples(save_pred = TRUE)
)
titanic_fits
titanic_fits %>% collect_metrics()

## roc curve
CV_ROC <-
  titanic_fits %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(survived, .pred_1, event_level = "second")
CV_ROC %>% autoplot()

## compare to test
titanic_test %>%
  add_column(
    predict(titanic_fit, new_data = titanic_test, type = "prob")
  ) %>%
  roc_auc(survived, .pred_1, event_level = "second")

## conbine plots
test_ROC <- titanic_test %>%
  add_column(
    predict(titanic_fit, new_data = titanic_test, type = "prob")
  ) %>%
  roc_curve(survived, .pred_1, event_level = "second") %>%
  add_column(id = "test")

bind_rows(CV_ROC, test_ROC) %>%
  ggplot(aes(1 - specificity, sensitivity, col = id)) + 
  geom_path() + 
  scale_color_brewer(palette = "Set2")

# case_study 5: Ridge Regression -- The office
pacman::p_load(tidyverse, tidymodels, tidytuesdayR,
               schrute, glmnet, plotly, vip, paletteer)
theme_set(theme_bw())

data("theoffice", package = "schrute")
office <- theoffice
office
length(unique(office$character))
office <- office %>%
  mutate(
    character = fct_lump_n(character, 20)
  )
unique(office$character)
office_long <- office %>%
  group_by(season, episode, character) %>%
  summarise(
    n_lines = n(),
    rating = mean(imdb_rating)
  )
office_long
office_wide <- office_long %>%
  pivot_wider(
    names_from = character,
    values_from = n_lines,
    values_fill = 0
  )
office_wide

# split data
office_split <- initial_split(office_wide, strata = rating)
office_train <- training(office_split)
office_test <- testing(office_split)

# pre-processing
office_recipe <- recipe(
  rating ~ ., data = office_train)
office_recipe

office_recipe <- office_recipe %>%
  update_role(season, episode, new_role = "ID")
office_recipe

office_recipe <- office_recipe %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
office_recipe

# just a check
office_recipe %>% prep() %>% juice()
office_recipe %>% prep() %>% juice() %>% select(rating)

# model
office_model <- linear_reg(
  mixture = 0,
  penalty = 0.1
) %>%
  set_engine("glmnet")

office_wf <-
  workflow() %>%
  add_recipe(office_recipe) %>%
  add_model(office_model)
office_wf

# fit
office_ridge <- 
  office_wf %>%
  fit(office_train)
office_ridge

p <- office_ridge %>%
  extract_fit_parsnip() %>%
  .$fit %>%
  tidy() %>%
  filter(
    term != "(Intercept)",
    lambda < 100
  ) %>%
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line()

ggplotly(p)

office_train %>%
  add_column(
    predict(office_ridge, new_data = office_train)
  ) %>% 
  ungroup() %>%
  rmse(rating, .pred)

office_test %>%
  add_column(
    predict(office_ridge, new_data = office_test)
  ) %>% 
  ungroup() %>%
  rmse(rating, .pred)

office_ridge %>%
  extract_fit_parsnip() %>%
  vi() %>%
  ggplot(aes(Importance, fct_reorder(Variable, Importance), fill = Sign)) +
  geom_col() +
  labs(y = "Character") + 
  scale_fill_paletteer_d("dutchmasters::milkmaid")

# case_study 7: Lasso Regression -- Hitters
pacman::p_load(tidyverse, tidymodels, vip, ggridges, viridis, magrittr, ISLR, janitor)
data("Hitters", package = "ISLR")
hitters <- as_tibble(Hitters)
hitters

## clean the data
hitters <- hitters %>% clean_names()
hitters

hitters <- hitters %>% drop_na()
hitters

## split the data
hitters_split <- initial_split(hitters, strata = salary)
hitters_train <- training(hitters_split)
hitters_test <- testing(hitters_split)

## create the recipe
hitters_recipe <- recipe(salary ~ ., data = hitters) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())
hitters_recipe %>% prep() %>% bake(new_data = NULL)
hitters_recipe %>% prep() %>% bake(new_data = NULL) %>% pull(salary)

## specify the model
hitters_model <- linear_reg(mixture = 1, penalty = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")
hitters_model

## workflow
hitters_wf <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_model)
hitters_wf

## Tuning
### penalty grid
hitters_grid <- tibble(penalty = seq(0.5, 50, 0.5))
hitters_grid

### cross validation folds
set.seed(2022)
hitters_cv <- vfold_cv(hitters_train, strata = salary)
hitters_cv

### tune the model
doParallel::registerDoParallel()
hitters_tune <- tune_grid(
  object = hitters_wf,
  resamples = hitters_cv,
  grid = hitters_grid
)
hitters_tune %>% collect_metrics()

### decide on a model
p <- hitters_tune %>% 
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(penalty, mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err))
p

plotly::ggplotly(p)

show_best(hitters_tune, metric = "rmse", n = 10)
(M1 <- select_best(hitters_tune, metric = "rmse"))
(M2 <- select_by_one_std_err(hitters_tune, metric = "rmse", penalty))
select_by_pct_loss(hitters_tune, metric = "rmse", penalty, limit = 5)

## Finalise the model
hitters_final_model <- hitters_wf %>% finalize_workflow(M1)
hitters_final_model

## viraible importance
hitters_final_model %>%
  fit(hitters_train) %>%
  extract_fit_parsnip() %>%
  vip()

## evaluate the performance of the model using test data
hitters_final_model %>% last_fit(hitters_split) %>% collect_metrics()

hitters_test %>%
  add_column(
    hitters_final_model %>%
      fit(data = hitters_train) %>% 
      predict(new_data = hitters_test)
  ) %>%
  rmse(salary, .pred)

# case_study 8: Decision Tree -- titanic
pacman::p_load(tidyverse, tidymodels, janitor, visNetwork, sparkline)

data('titanic_train', package = 'titanic')
titanic <- as_tibble(titanic_train)
titanic

titanic <- titanic %>% clean_names()
titanic

titanic <- titanic %>% 
  mutate_at(vars(survived, pclass, sex), factor)
titanic

titanic <- titanic %>%
  select(survived, pclass, sex, age, sib_sp, parch)
titanic

titanic_recipe <- recipe(survived ~ ., data = titanic)

titanic_model <- decision_tree(mode = "classification") %>%
  set_engine("rpart")

titanic_wf <- workflow() %>%
  add_recipe(titanic_recipe) %>%
  add_model(titanic_model)

titanic_fit <- titanic_wf %>% fit(titanic) %>% extract_fit_parsnip()
titanic_fit

plot(titanic_fit$fit)
text(titanic_fit$fit)

visTree(titanic_fit$fit)

jono <- tibble(
  pclass = "3",
  sex = "male",
  age = 51,
  sib_sp = 1,
  parch = 0
)

predict(titanic_fit, new_data = jono)

predict(titanic_fit, new_data = jono, type = "prob")

# case_study 9: SVM -- Board game
pacman::p_load(tidyverse, tidymodels, tidytuesdayR, harrypotter, kernlab)

tuesdata <- tidytuesdayR::tt_load('2019-03-12')
board <- tuesdata$board_games
write_rds(board, 'board.rds')

board <- read_rds('board.rds')
board

colnames(board)
board <- board %>% select(
  rating = average_rating,
  time = playing_time,
  year = year_published
)

board %>% ggplot(aes(time)) + geom_histogram(col = 'black', fill = 'orange')

60000/60/24

board <- board %>% filter(
  time > 0, time <= 360
)

board %>% ggplot(aes(time)) + geom_histogram(col = 'black', fill = 'orange')

board <- board %>% mutate(
  rating = cut_number(
    rating, 3,
    labels = c('pants', 'yeah', 'wow')
  )
)
board %>% ggplot(aes(year, time, col=rating)) + geom_point(alpha=0.7) + 
  scale_color_hp(house = 'ravenclaw', discrete = TRUE)

board %>% ggplot(aes(year, time, col=rating)) + geom_density_2d() + 
  scale_color_hp(house = 'ravenclaw', discrete = TRUE)

board_wf <- 
  workflow() %>%
  add_recipe(
    recipe(rating ~ ., data = board) %>%
      step_log(time)
  ) %>%
  add_model(
    svm_poly(mode = 'classification', degree = 2) %>%
      set_engine('kernlab')
  )

board_M1 <- board_wf %>% fit(board)
board_M1

board_M2 <- board_wf %>%
  update_model(
    svm_rbf(mode = 'classification') %>%
      set_engine('kernlab')
  ) %>% fit(board)
board_M2

board_M3 <- board_wf %>%
  update_model(
    rand_forest(mode = 'classification') %>%
      set_engine('ranger')
  ) %>% fit(board)
board_M3

newdata <- crossing(
  year = seq(1980, 2016, length = 100),
  time = seq(1, 360, length = 100)
)

newdata %>% 
  add_column(
    predict(board_M1, new_data = newdata)
  ) %>%
  ggplot(aes(year, time, fill = .pred_class)) + 
  geom_hex() + 
  scale_fill_hp(house = 'ravenclaw', discrete = TRUE)

newdata %>% 
  add_column(
    predict(board_M2, new_data = newdata)
  ) %>%
  ggplot(aes(year, time, fill = .pred_class)) + 
  geom_hex() + 
  scale_fill_hp(house = 'ravenclaw', discrete = TRUE)

newdata %>% 
  add_column(
    predict(board_M3, new_data = newdata)
  ) %>%
  ggplot(aes(year, time, fill = .pred_class)) + 
  geom_hex() + 
  scale_fill_hp(house = 'ravenclaw', discrete = TRUE)

board %>%
  add_column(
    predict(board_M1, new_data = board, type = 'prob')
  ) %>%
  mutate(type = 'SVM poly') %>%
  bind_rows(
    board %>%
      add_column(
        predict(board_M2, new_data = board, type = 'prob')
      ) %>%
      mutate(type = 'SVM radial')
  ) %>%
  bind_rows(
    board %>%
      add_column(
        predict(board_M3, new_data = board, type = 'prob')
      ) %>%
      mutate(type = 'RF')
  ) %>% 
  group_by(type) %>%
  roc_curve(rating, starts_with('.pred')) %>% 
  autoplot()

# case_study 11: PCA -- spotify
pacman::p_load(tidyverse, tidymodels, tidytuesdayR, paletteer, corrplot, tidytext)
spotify <- read_rds('spotify.rds')
spotify <- spotify %>% select(danceability:duration_ms, genre = playlist_genre)
spotify %>% select(danceability:duration_ms) %>%
  cor(use = 'pairwise.complete.obs') %>% corrplot()

spotify_prep <- 
  spotify %>% recipe(
    genre ~.
  ) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 4) %>%
  prep()
spotify_prep %>% tidy(n = 2) %>%
  filter(component %in% c('PC1', 'PC2', 'PC3', 'PC4')) %>%
  group_by(component) %>%
  top_n(5) %>% mutate(
    terms_order = reorder_within(terms, abs(value), component)
  ) %>% 
  ggplot(aes(value, terms_order, fill = terms)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~component, scales = 'free_y') + 
  scale_y_reordered()

spotify_prep %>% juice() %>%
  ggplot(aes(PC1, PC2, col = genre)) + 
  geom_point(alpha = 0.7) + 
  scale_color_paletteer_d("calecopal::superbloom3")

spotify_prep %>% juice() %>%
  ggplot(aes(PC1, fill = genre)) + 
  geom_density(alpha = 0.5) + 
  scale_color_paletteer_d("calecopal::superbloom3")

sdev <- spotify_prep$step[[2]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)
percent_variation

tibble(
  component = str_c('PC', 1:length(percent_variation)),
  percent_var = percent_variation
) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_var)) +
  geom_col() + 
  scale_color_paletteer_d("calecopal::superbloom3") +
  labs(x = NULL, y= "Percent variance explained by each PCA component")
