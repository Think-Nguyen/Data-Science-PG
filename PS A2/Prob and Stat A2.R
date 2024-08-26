library(tidyverse)

q2 <- tibble(
  X = c(2:7),
  Pr = c(3/14, 5/21, 1/14, 1/7, 4/21, 1/7)
)

q2 %>%
  ggplot(aes(x = X, y = Pr)) +
  geom_bar(stat="identity", width=0.7, color='black', fill='lightblue') +
  labs(x = 'X',
       y = 'Probability',
       title="Probability Mass Function (PMF) of X",) +
  # scale_x_discrete(breaks = 2:7) +
  theme(text = element_text(size = 40)) +
  theme_minimal()

# Suppose your tibble is named 'data' and it has a column named 'probability'
# with the probabilities you want to calculate the CDF from.

# Sample tibble
library(tibble)

# Sample data
data <- tibble(probability = c(0.1, 0.2, 0.3, 0.4))

# Calculate CDF
data$cdf <- cumsum(data$probability)


