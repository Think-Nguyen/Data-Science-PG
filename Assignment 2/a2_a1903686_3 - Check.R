library(tidyverse)

get_QDA <- function(x, y, x0) {
  # Load necessary libraries
  if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
  library(tidyverse)
  
  if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  library(dplyr)
  
  # Check input types
  if (!is.vector(x)) {
    stop("x should be a vector")
  }
  if (!is.vector(y)) {
    stop("y should be a vector")
  }
  if (!is.vector(x0)) {
    stop("x0 should be a vector")
  }
    
  # Check input values
  if (length(x) != length(y)) {
    stop("x and y should have a same length.")
  }
  if (length(unique(y)) < 2) {
    stop("y should have at least 2 levels.")
  }
    
  # Create dataframe and remove missing values
  df <- tibble(x, y) %>% drop_na()
  
  # Check each level has at least 2 observations and non-zero variance
  df_check <- df %>%
    group_by(y) %>%
    summarise(
      count = n(),
      variance = var(x),
      .groups = 'drop'
    )
  
  if(any(df_check$count < 2)) {
    stop("Each level of y should have at least 2 observations.")
  }
  if(any(df_check$variance == 0)) {
    stop("Each level of y should have non-zero variance.")
  }
  
  # Get estimates
  N <- length(x)
  K <- length(unique(y))
  
  df <-
    df %>%
    group_by(y) %>%
    mutate(
      mu = mean(x),
      pi = n() / N,
      s2 = var(x)
    )
  
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
  

x <- 8:0
y <- rep(LETTERS[26:24], each = 3)
x0 <- c(7,5)
get_QDA(x = x, y = y, x0 = x0)

# 1. Basic case
x <- c(1, 2, 3, 4, 5, 6)
y <- c("A", "A", "A", "B", "B", "B")
x0 <- c(2.5, 4)
get_QDA(x = x, y = y, x0 = x0)
# Expected Output:
# [1] "A" "B"

# 2. Multiple levels and variance
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c("A", "A", "A", "B", "B", "C", "C", "C")
x0 <- c(2, 4, 6)
get_QDA(x = x, y = y, x0 = x0)
# Expected Output:
# [1] "A" "B" "C"

# 3. Insufficient observations
x <- c(1, 2, 3, 4, 5, 6)
y <- c("A", "A", "A", "B", "B", "B")
x0 <- c(2, 4)
get_QDA(x = x, y = y, x0 = x0)
# Expected Output:
# Error: Each level of y must have at least 2 observations.

# 4. Zero variance
x <- c(1, 1, 3, 4, 5, 6)
y <- c("A", "A", "B", "B", "C", "C")
x0 <- c(2, 4)
get_QDA(x = x, y = y, x0 = x0)
# Expected Output:
# Error: Each level of y must have at least 2 observations.

