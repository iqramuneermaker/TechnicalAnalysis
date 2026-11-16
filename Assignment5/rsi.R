# rsi.R | Relative Strength Index | BDA400 A5 | Iqra
# RSI = 100 - 100 / (1 + avg_gain / avg_loss), with Wilder smoothing.
# Positions 1..period stay NA: no full window exists there yet.

rsi <- function(data, period) {
  diff_values <- diff(data)
  gains <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values > 0, 0, abs(diff_values))
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  rsi_values
}

# Demo:
# prices <- c(52, 54, 51, 57, 59, 56, 61, 63, 60, 66)
# rsi(prices, period = 5)
