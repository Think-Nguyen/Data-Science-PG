library(tidyverse)
library(tidymodels)
library(readxl)

rbc <- read_excel('rbc_clean.xlsx')
rbc <- as_tibble(rbc)

# Drop NA
rbc <- rbc %>% filter(fitness != 'NA')
rbc

# Tidy
rbc <- rbc %>%
  mutate(sex = recode(sex,
                      'male' = 'M',
                      'female' = 'F'),
         country = factor(country),
         fitness = as.double(fitness)
  )
rbc

# Drop country '4'
rbc %>% count(country)
rbc <- rbc %>% filter(country != '4')
rbc %>% count(country)

# Filter fitness and RBC
rbc <- rbc %>% filter(fitness < 100 & RBC != 0) 
rbc

# Answer question
# 1. Observations
rbc

# 2. Male
rbc %>% count(sex)

# 3. Country 2
rbc %>% filter(country == '3')

# 4. Mean RBC
rbc %>% summary()

rbc
