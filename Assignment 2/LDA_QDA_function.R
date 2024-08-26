library(tidyverse)
library(dplyr)

# LDA FUNCTION
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
  s2 <- sum((df$x - df$mu)**2) / (N - K)
  mu <- df %>% slice(1) %>% pull(mu)
  pi <- df %>% slice(1) %>% pull(pi)
  # Calculate delta
  delta <- numeric(K)
  for(i in 1:K){
    delta[i] <- x0 * mu[i] / s2 - mu[i]ˆ2 / (2*s2) + log(pi[i])
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


# QDA FUNCTION
jono_QDA <- function(x, y, x0, verbose = FALSE){
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
      pi = n() / N,
      s2 = var(x)
    )
  
  mu <- df %>% slice(1) %>% pull(mu)
  pi <- df %>% slice(1) %>% pull(pi)
  s2 <- df %>% slice(1) %>% pull(s2)
  
  # Calculate delta
  delta <- numeric(K)
  for(i in 1:K){
    delta[i] <- -0.5 * log(s2[i]) - 0.5 * ((x0 - mu[i])^2 / s2[i]) + log(pi[i])
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


get_QDA <- function(x, y, x0) {
  # Load required library
  if (!requireNamespace("MASS", quietly = TRUE)) {
    install.packages("MASS")
  }
  library(MASS)
  
  # Convert inputs to data frame
  data <- data.frame(x = x, y = y)
  
  # Fit QDA model
  qda_model <- qda(y ~ x, data = data)
  
  # Create new data frame for prediction
  new_data <- data.frame(x = x0)
  
  # Predict classes
  predictions <- predict(qda_model, new_data)$class
  
  return(predictions)
}

get_QDA <- function(x, y, x0){
  # Load necessary libraries
  if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
  library(tidyverse)
  
  if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  library(dplyr)
  
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
      pi = n() / N,
      s2 = var(x)
    ) %>%
    ungroup()
  
  # Extract the first row of each group for parameters
  params <- df %>% distinct(y, mu, pi, s2)
  
  mu <- params$mu
  pi <- params$pi
  s2 <- params$s2
  class_labels <- params$y
  
  # Calculate delta
  delta <- matrix(numeric(length(x0) * K), ncol = K)
  
  for(i in 1:K){
    delta[, i] <- -0.5 * log(s2[i]) - 0.5 * ((x0 - mu[i])^2 / s2[i]) + log(pi[i])
  }
  
  # Determine the predicted class for each x0
  max_delta_indices <- apply(delta, 1, which.max)
  predicted_classes <- class_labels[max_delta_indices]
  
  return(predicted_classes)
}

# Example usage
x <- 8:0
y <- rep(LETTERS[26:24], each = 3)
x0 <- c(7, 5)
get_QDA(x = x, y = y, x0 = x0)
# [1] "Z" "Y"
