library(tidyverse)
setwd("C:/Users/nguye/Music/2024 Tri 2_Using Machine Learning Tools/Assignment 1/")
df <- read.csv('SeouLBikeData.csv')
check <- as_tibble(df[0:1,])
check

df_cleaned <- read.csv('SeoulBikeData_Cleaned.csv')

df_imputed <- read.csv('SeoulBikeData_imputed.csv')
