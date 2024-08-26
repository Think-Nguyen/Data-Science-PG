pacman::p_load(tidyverse, tidymodels, paletteer, ISLR)
# week 2: regression and classification
## show that rsq and rmse go different directions
set.seed(2020)
Y <- rep(0:1, 10)
X <- matrix(rnorm(20*20), ncol = 20)
1:20 %>%
  map_df(~broom::glance(lm(Y ~ X[, 1:.x])), .id = "i") %>%
  mutate(i = as.numeric(i)) %>%
  select(R2 = r.squared, RSE = sigma, NP = i) %>%
  gather(key = "measure", value = value, -NP) %>%
  ggplot(aes(NP, value, col = measure)) + geom_point() +
  scale_color_paletteer_d("dutchmasters::milkmaid")

## Manual logistic regression (classification)
beetles <- read_csv('beetle.csv')
get_b0 <- function(y, n, X){
  v <- log((y + 0.5) / (n - y + 0.5))
  b0 <- solve(t(X) %*% X) %*% t(X) %*% v
  return(b0)
}X
<- cbind(1, beetles$Concentration)
b0 <- get_b0(y = beetles$Dead, n = beetles$Exposed, X = X)
b0

update_b <- function(bt, y, n, X){
  eta <- X %*% bt
  pi <- exp(eta) / (1 + exp(eta))
  mu <- n * pi
  D <- diag(as.numeric(n * pi * (1 - pi)))
  b <- bt + solve(t(X) %*% D %*% X)%*% t(X) %*% (y - mu)
  return(b)
}
update_b(bt = b0, y = beetles$Dead, n = beetles$Exposed, X = X)

# week 4: EDA
## histgrams of bill_length_mm of each species
pacman::p_load(palmerpenguins)
data(penguins, package = "palmerpenguins")
penguins

ggplot(penguins, aes(bill_length_mm, fill = species)) +
  geom_histogram(alpha = 0.5, position = "identity")

## LDA implementation
jono_LDA <- function(x, y, x0, verbose = FALSE){
  # Get estimates
  N <- length(x)
  K <- length(unique(y))
  df <- tibble(x, y)
  # Remove missing values
  df <- df %>% drop_na()
  df <-
    df %>%
    group_by(y) %>%
    mutate(
      mu = mean(x),
      pi = n() / N
    )
  s2 <- sum((df$x - df$mu)^2) / (N - K)
  mu <- df %>% slice(1) %>% pull(mu)
  pi <- df %>% slice(1) %>% pull(pi)
  # Calculate delta
  delta <- numeric(K)
  for(i in 1:K){
    delta[i] <- x0 * mu[i] / s2 - mu[i]^2 / (2*s2) + log(pi[i])
  }
  if(verbose){
    return(
      list(
        mu = mu,
        pi = pi,
        s2 = s2,
        delta = delta
      ))
  }
  return(delta)
}
jono_LDA(penguins$bill_length_mm, penguins$species,
         x0 = 40, verbose = TRUE)

## Built-in LDA
penguins_LDA <-
  discrim_linear() %>%
  set_engine("MASS") %>%
  fit(species ~ bill_length_mm, data = penguins)
penguins_LDA

predict(penguins_LDA, new_data = tibble(bill_length_mm = 40))

penguins_LDA <-
  discrim_linear() %>%
  set_engine("MASS") %>%
  fit(species ~ bill_length_mm + bill_depth_mm, data = penguins)
penguins_LDA

## QDA
### regularized QDA
pacman::p_load(klaR)
penguin_QDA <- discrim_regularized(
  frac_common_cov = 0,
  frac_identity = 0
) %>%
  set_engine("klaR") %>%
  fit(species ~ bill_length_mm + bill_depth_mm, data = penguins)
penguin_QDA

## Naive Bayes
pacman::p_load(mlbench, naivebayes)
data(HouseVotes84, package = "mlbench")
head(HouseVotes84)

housevotes84_NB <-
  naive_Bayes() %>%
  set_engine("naivebayes") %>%
  fit(Class ~ V5 + V9, data = HouseVotes84)
housevotes84_NB$fit$prior
HouseVotes84 %>%
  count(Class) %>%
  mutate(p = n / sum(n))
housevotes84_NB$fit$tables$V5
HouseVotes84 %>%
  count(Class, V5) %>%
  group_by(Class) %>%
  mutate(N = sum(n),
         p = n / N)

## Laplace Smoothing
df <-
  tibble(class = factor(rep(LETTERS[1:2], each = 4)),
         x1 = factor(c("N","N","Y","Y","N","N","N","N")),
         x2 = factor(c("a","b","b","b","b","a","a","a")))
df

naive_Bayes() %>%
  set_engine("naivebayes") %>%
  fit(class ~ ., data = df) %>%
  pluck("fit", "tables", "x1")

df %>%
  group_by(class, x1) %>%
  summarise(n=n()) %>%
  complete(x1, fill = list(n = 0)) %>%
  group_by(class) %>%
  mutate(N = sum(n),
         p = n / N)

naive_Bayes(Laplace = 1) %>%
  set_engine("naivebayes") %>%
  fit(class ~ ., data = df) %>%
  pluck("fit", "tables", "x1")

df %>%
  group_by(class, x1) %>%
  summarise(n=n()) %>%
  complete(x1, fill = list(n = 0)) %>%
  mutate( n = n + 1) %>%
  group_by(class) %>%
  mutate(N = sum(n),
         p = n / N)

## Validation
pacman::p_load(ISLR)
set.seed(2020)
data(Auto, package = "ISLR")
Auto_split <- initial_split(Auto, strata = mpg)
Auto_split
auto_train <- training(Auto_split)
auto_test <- testing(Auto_split)
auto_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ horsepower, data = auto_train)
auto_train %>%
  add_column(
    predict(auto_lm, new_data = auto_train)
  ) %>%
  yardstick::rmse(truth = mpg, estimate = .pred)
auto_test %>%
  add_column(
    predict(auto_lm, new_data = auto_test)
  ) %>%
  yardstick::rmse(truth = mpg, estimate = .pred)

## Cross Validation
data(mpg)
auto_cv <- vfold_cv(Auto, v = 5, strata = mpg)
auto_cv
auto_recipe <- recipe(mpg ~ horsepower, data = auto_train)
auto_recipe
auto_model <- linear_reg() %>% set_engine("lm")
auto_model
auto_wf <- workflow() %>%
  add_recipe(auto_recipe) %>%
  add_model(auto_model)
auto_wf
auto_rsamples <- fit_resamples(
  auto_wf,
  resamples = auto_cv
)
auto_rsamples %>% collect_metrics()
auto_rsamples$.metrics[[1]]

### Hand code
train_1 <- analysis(auto_cv$splits[[1]])
test_1 <- assessment(auto_cv$splits[[1]])
model_1 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg~horsepower, data = train_1)
test_1 %>%
  add_column(
    predict(model_1, new_data = test_1)
  ) %>%
  yardstick::rmse(truth = mpg, estimate = .pred)

## Bootstrap
df_BS <- bootstraps(df, times = 1000)
df_BS

get_median <- function(split, ...){
  df <- analysis(split)
  return(
    tibble(
      term = "Median",
      estimate = median(df$x),
      std.error = NA
    )
  )
}
df_BS <- df_BS %>%
  mutate(
    medians = map(splits, get_median)
  )
df_BS
T_obs <- median(df$x)
T_star <- df_BS %>% unnest(medians) %>% pull(estimate)
T_star[1:10]
### Percentile
quantile(T_star, c(0.025, 0.975))
int_pctl(df_BS, medians)
### Normal
bias <- mean(T_star) - T_obs
SE <- sqrt(var(T_star))
c(T_obs - bias - 1.96 * SE,T_obs - bias + 1.96 * SE)

# week 5: Model Selection and Ridge Regression
## Best Subset Selection
pacman::p_load(leaps)
data(mpg, package = "ggplot2")
mpg
mpg_full <- regsubsets(cty ~ displ + cyl + year, data = mpg)
summary(mpg_full)
coef(mpg_full, 1:3)

### Hitters Example
data("Hitters", package = "ISLR")
hitters_full <- regsubsets(Salary ~ ., data = Hitters)
plot(hitters_full)

## Forward Stepwise Selection
mpg_forward <- regsubsets(
  cty ~ displ + cyl + year, data = mpg, 
  method = "forward")
summary(mpg_forward)

## Backward Stepwise Selection
mpg_backward <- regsubsets(
  cty ~ displ + cyl + year, data = mpg, 
  method = "backward")
summary(mpg_backward)

## Choosing the optimal model
model_spec <- linear_reg() %>%
  set_engine("lm")
M1 <- model_spec %>% fit(cty ~ displ + cyl + year, data = mpg)
M2 <- model_spec %>% fit(cty ~ displ + cyl, data = mpg)
M3 <- model_spec %>% fit(cty ~ cyl, data = mpg)
list(M1 = M1$fit, M2 = M2$fit, M3 = M3$fit) %>%
  map_df(broom::glance, .id = "Model") %>%
  dplyr::select(Model, r.squared, adj.r.squared, AIC, BIC)

## Ridge Regression
data(Credit, package = "ISLR")
head(Credit)
credit_ridge <- linear_reg(mode = "regression",
                           mixture = 0,
                           penalty = 0) %>%
  set_engine("glmnet") %>%
  fit(Balance ~ Income + Limit + Student + Rating,
      data = Credit)
credit_ridge

## Singular Value Decomposition
# X = UDV^T, where the columns of U and V are orthogonal, i.e.,
# U^TU = I and V^TV = I
# and D is a diagonal matrix with diagonal entries
# d_1 ≥ d_2 ≥ ... ≥ d_p ≥ 0.
X <- matrix(1:6, nc = 2, nr = 3)
X
svd(X)
D <- diag(svd(X)$d)
U <- svd(X)$u
V <- svd(X)$v
U %*% D %*% t(V)
t(U)%*%U

# week 6: Lasso, PCR and PLS
## Lasso Regression
data("Hitters", package = "ISLR")
Hitters <- na.omit(Hitters)
head(Hitters)
hitter_lasso <- linear_reg(mode = "regression",
                           mixture = 1,
                           penalty = 0) %>%
  set_engine("glmnet") %>%
  fit(Salary ~ ., data = Hitters)
hitter_lasso

## Principle Component Regression
pacman::p_load(tidyverse, tidymodels, vip)
data(Hitters, package = "ISLR")
hitters <- Hitters %>%
  dplyr::select(where(is.numeric)) %>%
  drop_na() %>%
  as_tibble()
hitters
hitters_recipe <- hitters %>%
  recipe(Salary ~ .) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())
hitters_pcr <- linear_reg() %>%
  set_engine("lm")
hitters_cv <- vfold_cv(hitters)
hitters_wf <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_pcr)
hitters_wf
hitters_grid <- grid_regular(num_comp(range = c(1, 10)),
                             levels = 10)
hitters_grid
doParallel::registerDoParallel()
set.seed(2020)
hitters_tune <- tune_grid(hitters_wf,
                          resamples = hitters_cv,
                          grid = hitters_grid)
hitters_tune %>% autoplot()
show_best(hitters_tune, metric = "rmse")
best_model <- select_best(hitters_tune, metric = "rmse")
best_model
hitters_wf %>% finalize_workflow(parameters = best_model) %>% fit(hitters)

## Partial Least Squares
pacman::p_load(plsmod, mixOmics)
hitters_pls <- pls(mode = 'regression',
                   num_comp = tune()) %>%
  set_engine("mixOmics")
hitters_pls
hitters_pls_recipe <- hitters %>%
  recipe(Salary ~ .) %>%
  step_normalize(all_predictors())
hitters_pls_wf <- workflow() %>%
  add_recipe(hitters_pls_recipe) %>%
  add_model(hitters_pls)
hitters_pls_wf

doParallel::registerDoParallel()
set.seed(2020)
hitters_pls_tune <- tune_grid(hitters_pls_wf,
                              resamples = hitters_cv,
                              grid = hitters_grid)

# week 7: Poly Splines and LOESS
## Polynomial Regression
data(Wage, package = "ISLR")
head(Wage)
wage_M1_recipe <-
  recipe(wage ~ age, data = Wage) %>%
  step_poly(age, degree = 4, options = list(raw = TRUE))
wage_M1_recipe %>% prep() %>% juice()
wage_M1_wf <-
  workflow() %>%
  add_recipe(wage_M1_recipe) %>%
  add_model(linear_reg() %>% set_engine("lm"))
wage_M1 <- wage_M1_wf %>% fit(Wage)
wage_M1 %>% tidy()

# Orthogonal Polynomials
wage_M2_recipe <-
  recipe(wage ~ age, data = Wage) %>%
  step_poly(age, degree = 4)
wage_M2_recipe %>% prep() %>% juice()
wage_M2_wf <-
  workflow() %>%
  add_recipe(wage_M2_recipe) %>%
  add_model(linear_reg() %>% set_engine("lm"))
wage_M2 <- wage_M2_wf %>% fit(Wage)
wage_M2 %>% tidy()

df <- tibble(age = 1:10)
recipe(~age, df) %>%
  step_poly(age, degree = 3, options = list(raw = TRUE)) %>%
  prep() %>% juice()
M <- recipe(~age, df) %>%
  step_poly(age, degree = 3) %>%
  prep() %>% juice()
M
M <- as.matrix(M)
round(t(M) %*% M, 1)

## Step Functions
recipe(~age, data = Wage) %>%
  step_cut(age, breaks = c(0, 30, 60, 90)) %>%
  prep() %>%
  juice() %>%
  add_column(original_age = Wage$age)

wage_bin <- workflow() %>%
  add_model(
    linear_reg() %>% set_engine("lm")
  ) %>%
  add_recipe(
    recipe(wage~age, data = Wage) %>%
      step_cut(age, breaks = c(0, 30, 60, 90))
  ) %>%
  fit(Wage)
wage_bin

## Basis Functions
h <- function(x, xi){ ifelse(x > xi, (x - xi)^3, 0) }
ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = h, geom = "line", args = list(xi = 1),
                aes(col = "xi = 1")) +
  stat_function(fun = h, geom = "line", args = list(xi = 3),
                aes(col = "xi = 3")) + labs(col = "Knot")

## Cubic Spline
recipe(wage ~ age, data = Wage) %>%
  step_spline_b(age, options = list(knots = c(30, 60))) %>%
  prep() %>%
  juice()

## Natural Spline
recipe(wage ~ age, data = Wage) %>%
  step_ns(age, deg_free = 3) %>%
  prep() %>%
  juice()

## LOOCV for Smoothing Splines
data(Wage, package = "ISLR")
head(Wage)
wage_smooth_spline <- smooth.spline(x = Wage$age,
                                    y = Wage$wage,
                                    df=17)
wage_smooth_spline_CV <- smooth.spline(x = Wage$age,
                                       y = Wage$wage,
                                       cv = TRUE)
wage_smooth_spline
wage_smooth_spline$df
wage_smooth_spline_CV$df
wage_smooth_spline$lev
sum(wage_smooth_spline$lev)

## KNN Regression Function
knn <- function(x0, train, k){
  train <-
    train %>%
    mutate(dist = (train$x - x0)^2) %>%
    top_n(n = k,wt = -dist) %>%
    summarise(hat_f = mean(y)) %>%
    pull(hat_f)
  return(train)
}
KNN <- Vectorize(knn, "x0")

# Epanechnikov Code
epanechnikov_quad <- function(x, x0, lamb){
  t <- abs(x - x0) / lamb
  D <- ifelse(abs(t) <= 1,
              3/4 * (1 - tˆ2),
              0)
  return(D)
}

# Nadaraya-Watson
nadaraya_watson <- function(x0, train, lamb){
  train <-
    train %>%
    mutate(
      K = epanechnikov_quad(x, x0, lamb),
      Ky = K * y)
  return(sum(train$Ky) / sum(train$K))
}
Nadaraya_watson <- Vectorize(nadaraya_watson, "x0")

## LOESS
### Kernel is tri-cube.
### Default polynomial is quadratic.
data(Wage, package = "ISLR")
head(Wage)
wage_loess <- loess(wage ~ age, data = Wage)
wage_loess

### choosing span
# Based on
# autoloess.R: compute loess metaparameters automatically
# Kyle Gorman <gormanky@ohsu.edu>
#' gives AICC for loess
#'
#'Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing
# parameter selection in nonparametric regression using an improved
# Akaike Information Criterion. Journal of the Royal Statistical
# Society B 60: 271–293.
#'
#' @param fit
#'
#' @return AICC
#' @export
#'
#' @examples
aicc.loess <- function(fit) {
  stopifnot(inherits(fit, 'loess'))
  # parameters
  n <- fit$n
  trace <- fit$trace.hat
  sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
  return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
}
#' Finds optimal span using AICC for loess
#'
#' @param fit
#' @param span
#'
#' @return optimal span
#' @export
#'
#' @examples
autoloess <- function(fit, span=c(.1, .9)) {
  stopifnot(inherits(fit, 'loess'), length(span) == 2)
  # loss function in form to be used by optimize
  f <- function(span){
    aicc.loess(stats::update(fit, span=span))
  }
  # find minimum span
  min_span <- optimize(f, span)$minimum
  # find best loess according to loss function
  fit <- stats::update(fit, span = min_span)
  return(fit)
}
wage_loess_2 <- autoloess(wage_loess)
wage_loess_2
wage_loess_2$pars$span
wage_loess$pars$span

# week 8: MARS and CART
## Multivariate Adaptive Regression Spline (MARS)
### Hinge Functions
hinge <- function(x, c, type = '+'){
  if(type == "+"){
    return(pmax(0, x - c))
  } else (
    return(pmax(0, c - x))
  )
}

### Backronym is “Enhanced Adaptive Regression Through Hinges” (EARTH).
library(earth)
data(trees)
trees
head(trees)

### Fitting Earth
trees_mars <-
  mars(mode = 'regression') %>%
  set_engine("earth") %>%
  fit(Volume ~ ., data = trees)
summary(trees_mars$fit, style = "pmax")
trees_mars2 <-
  mars(mode = 'regression', prod_degree = 2) %>%
  set_engine("earth") %>%
  fit(Volume ~ ., data = trees)
summary(trees_mars2$fit, style = "pmax")
trees_mars %>% vip()

### Generalized Additive Models (GAMs)
data(Wage, package = "ISLR")
head(Wage)
wage_gam2 <- gam(Wage ~ s(year, 4) + s(age, 5) + education,
                 data = Wage)
plot(wage_gam2, se = TRUE, col = "blue", terms = "s(year, 4)")
plot(wage_gam2, se = TRUE, col = "blue", terms = "s(age, 5)")
plot(wage_gam2, se = TRUE, col = "blue", terms = "education")
wage_gam3 <- gam(Wage ~ s(year, 4) + lo(age, span = 0.7) + education,
                 data = Wage)
plot(wage_gam3, se = TRUE, col = "blue", terms = "s(year, 4)")
plot(wage_gam3, se = TRUE, col = "blue", terms = "lo(age, span = 0.7)")
plot(wage_gam3, se = TRUE, col = "blue", terms = "education")

### Non-Linear Regression
data("L.minor", package = "nlstools")
L.minor
L.minor_fit <- nls(
  rate ~ Vm * conc / (K + conc),
  data = L.minor,
  start = list(
    K = 20, Vm = 120
  )
)
summary(L.minor_fit)
confint(L.minor_fit)
data("Puromycin")
Puromycin
Puromycin_fit_1 <- nls(
  rate ~ Vm[state] * conc / (K[state] + conc),
  data = Puromycin,
  start = list(
    K = c(0.1, 0.1), Vm = c(200, 200)
  )
)
summary(Puromycin_fit_1)
Puromycin_fit_2 <- nls(
  rate ~ Vm * conc / (K[state] + conc),
  data = Puromycin,
  start = list(
    K = c(0.1, 0.1), Vm = 200
  )
)
Puromycin_fit_3 <- nls(
  rate ~ Vm[state] * conc / (K + conc),
  data = Puromycin,
  start = list(
    K = 0.1, Vm = c(200, 200)
  )
)
Puromycin_fit_4 <- nls(
  rate ~ Vm * conc / (K + conc),
  data = Puromycin,
  start = list(
    K = 0.1, Vm = 200
  )
)
anova(Puromycin_fit_1, Puromycin_fit_4)
anova(Puromycin_fit_1, Puromycin_fit_2)
anova(Puromycin_fit_1, Puromycin_fit_3)

## Regression Trees
data(Hitters, package = "ISLR")
head(Hitters)
hitters_tree <-
  decision_tree(mode = 'regression', tree_depth = 2) %>%
  set_engine('rpart') %>%
  fit(Salary ~ Years + Hits, data = Hitters)
hitters_tree
plot(hitters_tree$fit)
text(hitters_tree$fit)
visTree(hitters_tree$fit)
Hitters %>%
  filter(Years < 4.5) %>%
  summarise(mean(Salary, na.rm = TRUE))
Hitters %>%
  filter(Years >= 4.5, Hits < 117.5) %>%
  summarise(mean(Salary, na.rm = TRUE))
Hitters %>%
  filter(Years >= 4.5, Hits >= 117.5) %>%
  summarise(mean(Salary, na.rm = TRUE))

### decision tree workflow
hitters_tree_wf <-
  workflow() %>%
  add_recipe(
    recipe(Salary ~ Years + Hits, data = Hitters)
  ) %>%
  add_model(
    decision_tree(mode = 'regression', tree_depth = tune()) %>%
      set_engine('rpart')
  )
set.seed(2020)
hitters_tree_tune <- tune_grid(
  hitters_tree_wf,
  resamples = vfold_cv(Hitters),
  grid = grid_regular(tree_depth(), levels = 15)
)

## Classification Trees
heart <- read_csv("../data/Heart.csv")
heart <-
  heart %>%
  select(-...1) %>%
  mutate_if(is_character, factor)
heart
heart_tree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(AHD ~ ., data = heart)
heart_tree

# week 9: Random Forest and SVM
data(Boston, package = "MASS")
Boston <- as_tibble(Boston)
Boston
boston_rf <- rand_forest(mode = "regression") %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(medv ~ ., data = Boston)
boston_rf

## SVM
# Fit Linear Kernel
sim_SVM_linear <- svm_poly(mode = "classification") %>%
  set_engine("kernlab") %>%
  fit(Y ~ ., data = df)
sim_SVM_linear

# Fit Radial Kernel
sim_SVM_radial <- svm_rbf(mode = "classification") %>%
  set_engine("kernlab") %>%
  fit(Y ~ ., data = df)
sim_SVM_radial

# week 10: PCA and clustering
## PCA
data("USArrests")
head(USArrests)
US_arrest_PCA <- recipe(~., USArrests) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric()) %>%
  prep()
tidy(US_arrest_PCA, n = 2)
US_arrest_PCA %>% juice()
eigen(cor(USArrests))
sd <- US_arrest_PCA$steps[[2]]$res$sdev
sd^2
USArrests %>%
  mutate_all(scale) %>%
  as.matrix() %*%
  eigen(cor(USArrests))$vectors %>%
  head()
data("BreastCancer", package = "mlbench")
breast_cancer <- tibble(BreastCancer)
breast_cancer
breast_cancer <- breast_cancer %>%
  drop_na() %>%
  mutate_at(vars(Cl.thickness:Mitoses), as.numeric)

## K-means clustering
set.seed(2020)
df <- tibble(
  cluster = rep(1:2, each = 50)
) %>%
  mutate(x1 = rnorm(nrow(.), mean = (cluster == 1) * 3),
         x2 = rnorm(nrow(.)))
df$cluster <- factor(sample(df$cluster))
get_centroids <- function(df) {
  df %>%
    group_by(cluster) %>%
    summarise(x1 = mean(x1), x2 = mean(x2))
}
centroids <- get_centroids(df)
reassign_points <- function(df, centroids) {
  df %>%
    mutate(
      d1 = (x1 - as.numeric(centroids[1,2]))^2
      + (x2 - as.numeric(centroids[1, 3]))^2,
      d2 = (x1 - as.numeric(centroids[2,2]))^2
      + (x2 - as.numeric(centroids[2, 3]))^2
    ) %>%
    mutate(
      cluster = factor(ifelse(d1 < d2, 1, 2))
    )
}
df <- reassign_points(df, centroids)
old_cluster <- NULL
while (!identical(df$cluster, old_cluster)) {
  old_cluster <- df$cluster
  centroids <- get_centroids(df)
  print(centroids)
  df <- reassign_points(df, centroids)
}
data("penguins", package = "palmerpenguins")
penguins
clean_penguins <- penguins %>%
  drop_na()
clean_penguins
penguins_kmeans <- kmeans(clean_penguins[, 3:4],
                          centers = 3)
tidy(penguins_kmeans)
glance(penguins_kmeans)

## Hierarchical Clustering
set.seed(2020)
reduce_penguins <-
  clean_penguins %>%
  sample_frac(0.1)
reduce_penguins
d <- dist(reduce_penguins[3:6])
penguin_hc <- hclust(d)
plot(penguin_hc, cex = 0.6, hang = -1)
plot(penguin_hc, cex = 0.6, hang = -1,
     labels = reduce_penguins$species)
plot(penguin_hc, cex = 0.6, hang = -1,
     labels = reduce_penguins$species)
rect.hclust(penguin_hc, 3)
plot(penguin_hc, cex = 0.6, hang = -1,
     labels = cutree(penguin_hc, 3))

# week 11: Multi-dimensional Scaling and EM Algorithm
as.matrix(dist(df))
as.matrix(dist(df, method = "manhattan"))
as.matrix(dist(df, method = "minkowski", p = 3))

## Classical Scaling
X <- as.matrix(df)
X
B <- X %*% t(X)
B
D <- matrix(0, nr = 4, nc = 4)
for (i in 1:4) {
  for (j in 1:4) {
    D[i,j] <- B[i,i] + B[j,j] - 2 * B[i,j]
  }
}
D
(as.matrix(dist(X)))^2
B <- matrix(0, nc = 4, nr = 4)
for (i in 1:4) {
  for (j in 1:4) {
    B[i,j] <- -0.5*(D[i,j] - mean(D[i,]) - mean(D[,j]) + mean(D))
  }
}
B
D <- D - rep(colMeans(D), 4)
D <- D - rep(rowMeans(D), 4)
B <- -0.5 * D
B
B_eig <- eigen(B)
B_eig
lambda_1 <- B_eig$values[1]
e_1 <- B_eig$vectors[, 1]
f1 <- sqrt(lambda_1) * e_1
f1
lambda_2 <- B_eig$values[2]
e_2 <- B_eig$vectors[, 2]
f2 <- sqrt(lambda_2) * e_2
f2
UK <- readxl::read_excel(glue::glue("{wdpath}/data/GB.xlsx"))
UK[1:5, 1:5]
UK <- UK[,-1]
UK <- as.matrix(UK)
diag(UK) <- 0
UK[1:5, 1:5]
coords <- cmdscale(UK)
coords <- tibble(x = coords[, 1], y = coords[, 2], name = colnames(UK))
coords %>% ggplot(aes(x, y)) + geom_point()
data(Groceries, package = "arules")
Groceries <- as(Groceries, "matrix")*1
dim(Groceries)
Groceries[1, which(Groceries[1, ]==1)]

## Jarcard
groceries_jac <- dissimilarity(t(Groceries),
                               method = "jaccard")

as.matrix(groceries_jac)[1:5, 1:5]

groceries_MDS <- cmdscale(groceries_jac)

## EM Algorithm
# Initial Guess
pi <- 0.5
m1 <- 1
m2 <- 10
s <- sd(df$y)
theta <- c(m1 = m1, m2 = m2, s = s, pi = pi)
theta

# Expectation Step
E_step <- function(y, theta) {
  top <- theta['pi'] * dnorm(y, mean = theta['m2'],
                             sd = theta['s'])
  bottom <- (1 - theta['pi']) * dnorm(y, mean = theta['m1'],
                                      sd = theta['s']) + top
  return(top/bottom)
}
df <- df %>%
  mutate(g = E_step(y, theta))
df

# Maximisation Step
M_step <- function(g, y) {
  n <- length(y)
  m1 <- sum((1 - g) * y) / sum(1 - g)
  m2 <- sum(g * y) / sum(g)
  s <- sqrt(
    sum((1 - g) * (y - m1)ˆ2 + g * (y - m2)ˆ2) / (n - 2)
  )
  pi <- sum(g) / length(g)
  return(c(m1 = m1, m2 = m2, s = s, pi = pi))
}
theta <- M_step(df$g, df$y)
theta

# Iteration
results <- NULL
old <- rep(Inf, 4)
while (sum((old - theta)ˆ2) > 0.0000001) {
  old <- theta
  df <- df %>% mutate(g = E_step(y, theta))
  theta <- M_step(df$g, df$y)
  results <- bind_rows(results, theta)
}

# Built-in EM Algorithm
pacman::p_load(mclust)
df_EM <- Mclust(df$y)
summary(df_EM, parameters = TRUE)
plot(df_EM, what = "BIC")
data("diabetes", package = "mclust")
head(diabetes)
diabetes_EM <- Mclust(diabetes[, -1])
plot(diabetes_EM, what = "BIC")
diabetes_EM$parameters$pro
diabetes_EM$parameters$mean
diabetes_EM$parameters$variance$sigma
