# stoch_rsi.R | Stochastic RSI | BDA400 A5 | Iqra
# Normalizes RSI into [0, 1]; %K smooths the normalized series with sma(),
# %D smooths %K. The NA warm-up values are excluded from min/max and dropped
# before smoothing. Source rsi.R and sma.R before this file.

if (!exists("rsi")) source("rsi.R")
if (!exists("sma")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  k_clean <- k_values[!is.na(k_values)]
  k_line <- sma(k_clean, k_period)
  d_line <- sma(k_line, d_period)
  list(k_line = k_line, d_line = d_line)
}

# Demo:
# prices <- c(52, 54, 51, 57, 59, 56, 61, 63, 60, 66)
# stoch_rsi(prices, period = 5, k_period = 3, d_period = 3)
