# macd.R | MACD | BDA400 A5 | Iqra
# macd = ema(short) - ema(long); signal = ema(macd); histogram = macd - signal.
# Source ema.R before using this file.

if (!exists("ema")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line
  list(macd_line = macd_line, signal_line = signal_line, histogram = histogram)
}

# Demo:
# prices <- c(120, 118, 123, 127, 125, 131, 136, 134, 139)
# macd(prices, short_period = 3, long_period = 6, signal_period = 2)
