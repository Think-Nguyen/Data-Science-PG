library(tidyverse)
library(dplyr)

get_ROC <- function(obs, A) {
  # Load necessary libraries
  if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
  library(tidyverse)
  
  if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  library(dplyr)
  
  # Check input types
  if (!is.factor(obs)) {
    stop("obs should be a factor.")
  }
  if (!is.numeric(A)) {
    stop("pred should be a numeric vector.")
  }
  if (length(obs) != length(A)) {
    stop("obs and A should have the same length.")
  }
  
  # Check input values
  if (length(levels(obs)) != 2 || !all(levels(obs) %in% c("A", "B"))) {
    stop("obs should have exactly two levels: 'A' and 'B'.")
  }
  if (any(A < 0 | A > 1)) {
    stop("A should contain values between 0 and 1.")
  }
  
  # Create a dataframe with 2 variables: 'obs' and 'pred'
  data <- tibble(
    obs = factor(obs),
    pred = A
  )
  
  # Sort predicted probabilities 
  data <- data %>% arrange(pred)
  
  # Initialize vectors for thresholds, specificity, and sensitivity
  thresholds <- c(-Inf, data$pred, Inf)
  sens <- numeric(length(thresholds))
  spec <- numeric(length(thresholds))
  
  # Calculate sensitivity and specificity at each threshold
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    tp <- sum(data$pred >= threshold & data$obs == "A")
    fp <- sum(data$pred >= threshold & data$obs == "B")
    fn <- sum(data$pred < threshold & data$obs == "A")
    tn <- sum(data$pred < threshold & data$obs == "B")
    
    sens[i] <- tp / (tp + fn)
    spec[i] <- tn / (tn + fp)
  }
  
  # Create a tibble with the results
  roc_tibble <- tibble(
    threshold = thresholds,
    specificity = spec,
    sensitivity = sens,
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn
  )
  
  # Remove duplicate rows
  roc_tibble <- roc_tibble[!duplicated(roc_tibble),]
  
  return(roc_tibble)
}


## Test function
df <- tibble(
  obs = rep(factor(c("A", "B")), each = 2),
  A = rep(c(0.8, 0.2), each = 2)
)

get_ROC(df$obs, df$A)

yardstick::roc_curve(df, truth = obs, A)
