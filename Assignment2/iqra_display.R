# iqra_display.R
# BDA400 Assignment 2 | Iqra
# Display utilities: a market-watch table like the ones on Yahoo Finance and
# the Globe and Mail markets page, per-stock OHLC tail views, and candlestick
# charts drawn with quantmod's chartSeries.

source("iqra_load_and_stats.R")

# One-line-per-stock market watch table (last close, change, percent change).
market_watch <- function(stocks) {
  rows <- lapply(names(stocks), function(sym) {
    cl <- as.numeric(Cl(stocks[[sym]]))
    n <- length(cl)
    data.frame(Symbol = sym,
               Last = round(cl[n], 2),
               Change = round(cl[n] - cl[n - 1], 2),
               PctChange = round(100 * (cl[n] / cl[n - 1] - 1), 2),
               YearLow = round(min(cl), 2),
               YearHigh = round(max(cl), 2))
  })
  do.call(rbind, rows)
}

# OHLCV tail view, like the Historical Data tab.
show_recent <- function(stocks, days = 5) {
  for (sym in names(stocks)) {
    cat("\n====", sym, "| last", days, "trading days ====\n")
    print(round(tail(stocks[[sym]], days), 2))
  }
}

# Candlestick chart per stock, saved to PNG via chartSeries.
save_candles <- function(stocks) {
  for (sym in names(stocks)) {
    png(paste0("candles_", gsub("\\.", "_", sym), ".png"), width = 900, height = 520)
    chartSeries(stocks[[sym]], name = sym, theme = chartTheme("white"),
                TA = "addSMA(n = 20, col = 'darkgreen')")
    dev.off()
  }
}

# ---- Run ----
stocks <- load_stock_data("portfolio.txt")
cat("\n============ MARKET WATCH ============\n")
print(market_watch(stocks), row.names = FALSE)
cat("\n============ STATISTICS (Close) ============\n")
print(portfolio_statistics(stocks))
show_recent(stocks)
save_candles(stocks)
cat("\nSaved candlestick charts for:", paste(names(stocks), collapse = ", "), "\n")
