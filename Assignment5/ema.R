# ema.R | Exponential Moving Average | BDA400 A5 | Iqra
# Recursive EMA seeded with the first observation; alpha = 2 / (period + 1).

ema <- function(data, period) {
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]                  # seed: first point
  for (i in seq_along(data)[-1]) {
    ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
  }
  ema_values
}

# Demo:
# prices <- c(23, 27, 25, 31, 29, 35, 38, 36, 40)
# ema(prices, period = 4)
