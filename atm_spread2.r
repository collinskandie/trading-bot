library(quantmod)

# Define function to get price of ATM option
getOptionPrice <- function(ticker, date) { # nolint
  getOptionChain(ticker, NULL, date)$calls[grep("ATM", getOptionChain(ticker, NULL, date)$calls$desc), "mid"] # nolint
}

# Set initial variables
start_date <- "2022-01-01"
end_date <- "2022-12-31"
call_Spread_Width <- 0.05 # nolint: object_name_linter.
putSpreadWidth <- 0.05 # nolint: object_name_linter
tqqqTicker <- "TQQQ" # nolint: object_name_linter
sqqqTicker <- "SQQQ" # nolint: object_name_linter
portfolio <- data.frame(date = as.Date(character()), tqqq_call_buy = numeric(), tqqq_call_sell = numeric(), # nolint: object_name_linter
                        sqqq_put_buy = numeric(), sqqq_put_sell = numeric(), profit_loss = numeric(), # nolint: object_name_linter
                        weekly_return = numeric())

# Loop over time period
for (date in seq(as.Date(start_date), as.Date(end_date), by="day")) {
  if (weekdays(date) == "Friday") {
    # Get mid-market rates for TQQQ call spread
    tqqq_call_buy_price <- getOptionPrice(tqqqTicker, date)
    tqqq_call_sell_price <- getOptionPrice(tqqqTicker, date) + call_Spread_Width # nolint
    # Get mid-market rates for SQQQ put spread
    sqqq_put_buy_price <- getOptionPrice(sqqqTicker, date)
    sqqq_put_sell_price <- getOptionPrice(sqqqTicker, date) - putSpreadWidth
      # Calculate profit/loss from previous week's position and add to portfolio
    if (nrow(portfolio) > 0) {
      previousWeek <- portfolio[nrow(portfolio), ]
      tqqq_call_profit_loss <- previousWeek$tqqq_call_sell - tqqq_call_buy_price
      sqqq_put_profit_loss <- sqqq_put_sell_price - previousWeek$sqqq_put_buy
      profit_loss <- tqqq_call_profit_loss + sqqq_put_profit_loss
      weekly_return <- profit_loss / previousWeek$investment
      portfolio[nrow(portfolio), "profit_loss"] <- profit_loss
      portfolio[nrow(portfolio), "weekly_return"] <- weekly_return
    }
    
    # Add current week's position to portfolio
    investment <- tqqq_call_buy_price + sqqq_put_buy_price
    portfolio <- rbind(portfolio, data.frame(date = date, tqqq_call_buy = tqqq_call_buy_price,
                                             tqqq_call_sell = tqqq_call_sell_price, sqqq_put_buy = sqqq_put_buy_price,
                                             sqqq_put_sell = sqqq_put_sell_price, profit_loss = NA, investment = investment,
                                             weekly_return = NA))
    cat("Bought TQQQ call spread and SQQQ put spread on", date, "\n")
  } else if (weekdays(date) %in% c("Saturday", "Sunday")) {
    # Do nothing on weekends
    next
  } else {
    # Check if options expiration or last working day of week
    if (format(date, "%Y%m") != format((date + 7), "%Y%m") | weekdays(date + 7) == "Saturday") {
      # Close position and update portfolio
      currentWeek <- portfolio[nrow(portfolio), ]
      tqqq_call_profit_loss <- currentWeek$tqqq_call_sell - getOptionPrice(tqqqTicker, date)
      sqqq_put_profit_loss <- getOptionPrice(sqqqTicker, date) - currentWeek$sqqq_put_sell
      profit_loss <- tqqq_call_profit_loss + sqqq_put_profit_loss
    }
    }
}    