get_kmeans <- function(x, k) {
  # Load necessary libraries
  if (!require(tidyverse)) install.packages('tidyverse', dependencies=TRUE)
  library(tidyverse)
  
  if (!require(dplyr)) install.packages('dplyr', dependencies=TRUE)
  library(dplyr)
  
  # Check input types
  if (!is.numeric(x) || !is.vector(x)) {
    stop('x should be a numeric vector.')
  }
    
  # Check input values
  if (!is.numeric(k) || k < 1 || k > length(unique(x))) {
    stop('k should be a numeric value between 1 and the number of unique values in x.')
  }
  
  # Initialize centroids
  centroids <- sample(unique(x), k)
  
  # Create a function to calculate distance of centroids
  cal_distance <- function(x, centroids) {
    sapply(centroids, function(c) abs(x - c))
  }
  
  # Reassign centroids until stable
  repeat {
    # Calculate distances
    distances <- cal_distance(x, centroids)
    
    # Group distances
    clusters <- max.col(-distances)
    
    # Calculate new centroids
    new_centroids <- sapply(1:k, function(i) mean(x[clusters == i], na.rm = TRUE))
    
    # Check for convergence
    if (all(new_centroids == centroids)) break
    centroids <- new_centroids
  }
  
  # Get order of centroids
  order_centroids <- order(centroids, decreasing = TRUE)
  
  # Relabel clusters from order centroids
  relabeled_clusters <- sapply(clusters, function(cluster) which(order_centroids == cluster))
  
  return(relabeled_clusters)
}