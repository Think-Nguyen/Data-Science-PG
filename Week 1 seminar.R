get_mean <- function(x) {
  total <- 0
  n <- length(x)
  for (i in 1:n) {
    total <- total + x[i]
  }
  mean_x <- total / n
  return(mean_x)
}

get_mean(1:10)

get_M2 <- function(x) {
  total <- 0
  n <- length(x)
  for (i in 2:n) {
    mean_i <- get_mean(x[1] : x[i])
    mean_i_1 <- get_mean(x[1] : x[(i-1)])
    
    total <- total + (x[i] - mean_i_1) * (x[i] - mean_i)
  }
  return(total)
}

get_M2(1:10)

get_var <- function(x) {
  m2 <- get_M2(x)
  n <- length(x)
  v <- m2 / (n - 1)
  return(v)
}

get_var(1:10)


