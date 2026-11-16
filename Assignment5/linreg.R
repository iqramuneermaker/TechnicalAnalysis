# linreg.R | Linear Regression | BDA400 A5 | Iqra
# Least squares y = b0 + b1 * x over the window selected by regressionLength
# and regressionOffset (window logic exactly as in the pseudocode).

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  list(slope = slope, intercept = intercept, predicted_values = predicted_values)
}

# Demo:
# prices <- c(23, 27, 25, 31, 29, 35, 38, 36, 40)
# linreg(prices, regressionLength = 6, regressionOffset = 1)
