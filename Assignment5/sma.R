# sma.R | Simple Moving Average | BDA400 A5 | Iqra
# Average of each sliding window of 'period' points. Base R only.

sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  n_windows <- length(data) - period + 1
  sma_values <- numeric(n_windows)          # pre-allocate instead of growing
  for (i in seq_len(n_windows)) {
    window_vals <- data[i:(i + period - 1)]
    sma_values[i] <- sum(window_vals) / period
  }
  sma_values
}

# Demo:
# prices <- c(23, 27, 25, 31, 29, 35, 38, 36, 40)
# sma(prices, period = 4)
