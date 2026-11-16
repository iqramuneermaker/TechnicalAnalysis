# iqra_portfolio_dashboard.R
# BDA400 Assignment 6 | Technical Analysis using R, Visualization Phase
# Iqra
#
# Interactive portfolio dashboard for my TSX watchlist, built with shiny,
# ggplot2 and quantmod (data source: Yahoo Finance). Features:
#   * line, area or candlestick price chart with a date range picker and
#     daily / weekly / monthly aggregation
#   * SMA and EMA overlays with their own on/off switches AND adjustable
#     window lengths
#   * RSI and MACD panels on their own tabs, each toggleable
#   * a fast/slow SMA crossover trading rule with adjustable windows;
#     Buy and Sell bars are annotated on the chart and listed in a table
#
# Run with:  shiny::runApp("iqra_portfolio_dashboard.R")
# Needs:     install.packages(c("shiny", "ggplot2", "quantmod"))

library(shiny)
library(ggplot2)
library(quantmod)

watchlist <- c("RY.TO", "SHOP.TO", "ENB.TO", "CNR.TO", "BMO.TO")

# xts -> data frame, because ggplot2 layers want plain columns
as_price_df <- function(x) {
  data.frame(Date = as.Date(index(x)),
             Open = as.numeric(Op(x)), High = as.numeric(Hi(x)),
             Low = as.numeric(Lo(x)), Close = as.numeric(Cl(x)))
}

# A moving average needs at least n bars; on short windows (for example a
# monthly view of one year) TTR stops with an error, so this wrapper
# returns all NA instead and the chart simply shows no overlay.
safe_ma <- function(fun, x, n) {
  if (is.na(n) || n < 2 || n >= length(x)) rep(NA_real_, length(x)) else fun(x, n = n)
}

# Trading rule: fast SMA crossing the slow SMA.
# Crossing above -> Buy, crossing below -> Sell, otherwise Hold.
# Vectorised: compare today's state with yesterday's state.
signal_frame <- function(df, fast_n, slow_n) {
  df$fast <- safe_ma(SMA, df$Close, fast_n)
  df$slow <- safe_ma(SMA, df$Close, slow_n)
  above <- df$fast > df$slow
  prev <- c(NA, head(above, -1))
  df$Signal <- ifelse(is.na(above) | is.na(prev) | above == prev, "Hold",
                      ifelse(above, "Buy", "Sell"))
  df
}

ui <- fluidPage(
  titlePanel("TSX Watchlist Dashboard (BDA400 Assignment 6, Iqra)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("ticker", "Ticker:", choices = watchlist),
      dateRangeInput("date_range", "Date range:",
                     start = Sys.Date() - 540, end = Sys.Date()),
      radioButtons("frequency", "Bars:", c("Daily", "Weekly", "Monthly"),
                   inline = TRUE),
      selectInput("chart_type", "Chart type:",
                  c("Candlestick", "Line", "Area")),
      hr(),
      checkboxInput("show_sma", "SMA overlay", TRUE),
      numericInput("sma_n", "SMA window:", 20, min = 2, max = 200),
      checkboxInput("show_ema", "EMA overlay", FALSE),
      numericInput("ema_n", "EMA window:", 20, min = 2, max = 200),
      hr(),
      checkboxInput("annotate", "Annotate Buy / Sell bars", TRUE),
      numericInput("fast_n", "Crossover fast SMA:", 20, min = 2, max = 100),
      numericInput("slow_n", "Crossover slow SMA:", 50, min = 5, max = 250)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Price", plotOutput("price_plot", height = "460px")),
        tabPanel("RSI", plotOutput("rsi_plot", height = "300px")),
        tabPanel("MACD", plotOutput("macd_plot", height = "300px")),
        tabPanel("Signals", tableOutput("signal_table"))
      )
    )
  )
)

server <- function(input, output, session) {

  # one reactive fetch per ticker/date change, with graceful failure
  raw_data <- reactive({
    req(input$ticker)
    tryCatch(
      getSymbols(input$ticker, src = "yahoo",
                 from = input$date_range[1], to = input$date_range[2],
                 auto.assign = FALSE),
      error = function(e) NULL
    )
  })

  prices <- reactive({
    x <- raw_data()
    validate(need(!is.null(x), "Yahoo Finance did not return data, try again."))
    x <- switch(input$frequency,
                Weekly = to.weekly(x),
                Monthly = to.monthly(x),
                x)
    signal_frame(as_price_df(x), input$fast_n, input$slow_n)
  })

  output$price_plot <- renderPlot({
    df <- prices()
    p <- ggplot(df, aes(Date))

    p <- switch(input$chart_type,
      Line = p + geom_line(aes(y = Close), color = "#1A5C3A", linewidth = 0.6),
      Area = p + geom_area(aes(y = Close), fill = "#1A5C3A", alpha = 0.2) +
        geom_line(aes(y = Close), color = "#1A5C3A"),
      Candlestick = {
        up <- df$Close >= df$Open
        p + geom_linerange(aes(ymin = Low, ymax = High), color = "grey45",
                           linewidth = 0.3) +
          geom_segment(aes(xend = Date, y = Open, yend = Close),
                       color = ifelse(up, "#1A9850", "#D73027"), linewidth = 2)
      })

    if (isTRUE(input$show_sma))
      p <- p + geom_line(aes(y = safe_ma(SMA, Close, input$sma_n), color = "SMA"),
                         na.rm = TRUE, linewidth = 0.6)
    if (isTRUE(input$show_ema))
      p <- p + geom_line(aes(y = safe_ma(EMA, Close, input$ema_n), color = "EMA"),
                         na.rm = TRUE, linewidth = 0.6)
    p <- p + scale_color_manual(name = "Overlays",
                                values = c(SMA = "#E69F00", EMA = "#7B3294"))

    if (isTRUE(input$annotate)) {
      marks <- df[df$Signal != "Hold", ]
      if (nrow(marks) > 0)
        p <- p +
          geom_vline(xintercept = marks$Date, linetype = "dotted",
                     color = "grey60") +
          geom_label(data = marks, aes(y = Close, label = Signal),
                     size = 3, fontface = "bold",
                     fill = ifelse(marks$Signal == "Buy", "#D9F0D3", "#FDDBC7"))
    }

    p + labs(title = paste0(input$ticker, " (", input$frequency, ", ",
                            input$chart_type, ") with ", input$fast_n, "/",
                            input$slow_n, " SMA crossover rule"),
             x = NULL, y = "Price (CAD)") +
      theme_minimal(base_size = 13)
  })

  output$rsi_plot <- renderPlot({
    df <- prices()
    validate(need(nrow(df) > 15, "Not enough bars for RSI(14)."))
    df$RSI <- RSI(df$Close, 14)
    ggplot(df, aes(Date, RSI)) +
      geom_ribbon(aes(ymin = 30, ymax = 70), fill = "grey92") +
      geom_line(color = "#1A5C3A", na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed") +
      ylim(0, 100) +
      labs(title = paste("RSI(14) of", input$ticker), y = "RSI", x = NULL) +
      theme_minimal(base_size = 12)
  })

  output$macd_plot <- renderPlot({
    df <- prices()
    validate(need(nrow(df) > 35, "Not enough bars for MACD(12,26,9)."))
    m <- as.data.frame(MACD(df$Close, 12, 26, 9))
    df <- cbind(df, m)
    ggplot(df, aes(Date)) +
      geom_col(aes(y = macd - signal), fill = "grey70", na.rm = TRUE) +
      geom_line(aes(y = macd), color = "#1A5C3A", na.rm = TRUE) +
      geom_line(aes(y = signal), color = "#E69F00", na.rm = TRUE) +
      labs(title = paste("MACD(12,26,9) of", input$ticker),
           subtitle = "green = MACD, orange = signal, bars = histogram",
           y = "MACD", x = NULL) +
      theme_minimal(base_size = 12)
  })

  output$signal_table <- renderTable({
    df <- prices()
    out <- df[df$Signal != "Hold",
              c("Date", "Close", "fast", "slow", "Signal")]
    validate(need(nrow(out) > 0, "No crossover signals in this window."))
    out$Date <- format(out$Date)
    names(out) <- c("Date", "Close", "Fast SMA", "Slow SMA", "Signal")
    out
  })
}

shinyApp(ui, server)
