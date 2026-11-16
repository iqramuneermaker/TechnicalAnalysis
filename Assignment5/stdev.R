# stdev.R | Standard Deviation | BDA400 A5 | Iqra
# Population standard deviation per the assignment formula: the variance is the
# mean of squared deviations (divide by n). R's own sd() divides by n - 1, so it
# is close but not equal; that is intentional here, not an error.

stdev <- function(data) {
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value          # vectorized differences
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  sqrt(variance)
}

# Demo:
# prices <- c(23, 27, 25, 31, 29, 35, 38, 36, 40)
# stdev(prices)
