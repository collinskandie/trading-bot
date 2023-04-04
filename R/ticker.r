library(tidyverse)
library(quantmod)

# Define the tickers and expiration dates
tqqq <- "TQQQ"
sqqq <- "SQQQ"

# install.packages("timeDate")   # install the timeDate package
# library(timeDate)             # load the package into your R session
expiry_date <- as.Date("2023-03-31")   # use the nextFriday() function to calculate the expiry date

# Define the strike prices for the call and put spreads
strike_price_call <- round(Ad(getSymbols(tqqq, auto.assign = FALSE))[1] * 1.05, digits = 2)
strike_price_put <- round(Ad(getSymbols(sqqq, auto.assign = FALSE))[1] * 0.95, digits = 2)

# Define the mid-market rate for the call and put spreads
mid_market_call <- (round(Ad(getSymbols(tqqq, auto.assign = FALSE))[1], digits = 2) +
                    round(Ad(getSymbols(tqqq, auto.assign = FALSE))[2], digits = 2)) / 2
mid_market_put <- (round(Ad(getSymbols(sqqq, auto.assign = FALSE))[1], digits = 2) +
                   round(Ad(getSymbols(sqqq, auto.assign = FALSE))[2], digits = 2)) / 2

# Define the number of contracts to trade
num_contracts <- 10
order_call <- function(ticker, expiry_date, strike_price, mid_market_rate, num_contracts) 

# Place the order for the call spread
order <- order_call(tqqq, expiry_date, strike_price_call, mid_market_call, num_contracts)

# Place the order for the put spread
order_put <- function(ticker, expiry_date, strike_price, mid_market_rate, num_contracts) 
order <- order_put(sqqq, expiry_date, strike_price_put, mid_market_put, num_contracts)

# Define a function to close the position on the day of expiration or the last working day of that week
close_position <- function() {
  if (weekdays(Sys.Date()) == "Friday" & weekdays(expiry_date) == "Friday") {
    order <- close_order_call(tqqq, expiry_date)
    order <- close_order_put(sqqq, expiry_date)
  } else if (Sys.Date() == expiry_date) {
    order <- close_order_call(tqqq, expiry_date)
    order <- close_order_put(sqqq, expiry_date)
  }
}

# Define a function to record profit/loss from each sale in the portfolio
portfolio <- tibble(date = Sys.Date(),
                    ticker = c(tqqq, sqqq),
                    type = c("Call", "Put"),
                    strike_price = c(strike_price_call, strike_price_put),
                    mid_market_rate = c(mid_market_call, mid_market_put),
                    num_contracts = num_contracts,
                    total_cost = c(num_contracts * mid_market_call * 100, num_contracts * mid_market_put * 100))

# Define a function to update the weekly portfolio returns
portfolio_return <- function() {
  weekly_portfolio <- portfolio %>% filter(date >= floor_date(Sys.Date(), "week"))
  total_cost <- sum(weekly_portfolio$total_cost)
  current_value <- sum(num_contracts * mid_market_call * 100, num_contracts * mid_market_put * 100)
  weekly_return <- (current_value - total_cost) / total_cost
  return(weekly_return)
}
