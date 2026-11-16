# iqra_load_and_stats.R
# BDA400 Assignment 2 | Technical Analysis using R, Preliminary Stage | Iqra
# I chose five TSX-listed Canadian companies for my portfolio. The functions
# below read portfolio.txt, import each symbol from Yahoo Finance with
# quantmod, and compute the required statistics on the closing prices.

library(quantmod)

# Import every symbol in the portfolio file; returns a named list of xts
# objects. Yahoo uses the .TO suffix for Toronto Stock Exchange tickers.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = "2026-01-01", to = Sys.Date()) {
  symbols <- trimws(readLines(portfolio_file))
  symbols <- symbols[symbols != ""]
  stocks <- lapply(symbols, function(sym) {
    message("Fetching ", sym)
    getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  })
  names(stocks) <- symbols
  stocks
}

# Statistical mode: closes rounded to the nearest dollar, most frequent value.
# (Raw closing prices rarely repeat, so the mode needs a binning rule.)
mode_value <- function(x) {
  tab <- table(round(as.numeric(x)))
  as.numeric(names(which.max(tab)))
}

# Rolling mean without TTR: stats::filter() with equal weights gives the same
# result as a simple moving average and is part of base R's stats package.
rolling_mean <- function(x, n) {
  as.numeric(stats::filter(as.numeric(x), rep(1 / n, n), sides = 1))
}

# All required statistics for one stock, computed on the Close column.
calculate_statistics <- function(stock_xts, ma_days = 20) {
  closes <- as.numeric(Cl(stock_xts))
  ma <- rolling_mean(closes, ma_days)
  c(mean = mean(closes),
    mode = mode_value(closes),
    median = median(closes),
    st_dev = sd(closes),
    ma20_latest = tail(ma[!is.na(ma)], 1))
}

# Portfolio-wide statistics table (stocks as rows).
portfolio_statistics <- function(stocks) {
  round(t(sapply(stocks, calculate_statistics)), 4)
}
