library(quantmod)
source("getOptionChain.R")
library(tmap)
data("World") # note the capital W!
plot(st_geometry(World))

# Function to get the ATM option price
get_option_price <- function(ticker, date) {
  # Download the option chain data
  options_data <- getOptionChain(ticker, NULL)
  
  # Find the at-the-money option
  spot_price <- as.numeric(getQuote(ticker)$Last)
  options_data$Strike <- as.numeric(as.character(options_data$Strike))
  atm_option <- options_data[which.min(abs(options_data$Strike - spot_price)), ]
  
  # Return the mid-market price
  (as.numeric(atm_option$Ask) + as.numeric(atm_option$Bid)) / 2
}

# Define the option spreads
call_spread_width <- 0.05
put_spread_width <- 0.05

# Define the tickers
tqqq_ticker <- "TQQQ"
sqqq_ticker <- "SQQQ"

# Define the start and end dates
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")

# Loop over the weeks
week_start <- start_date - as.numeric(format(start_date, "%w")) + 5
week_end <- week_start + 6
portfolio_value <- 1000000  # starting portfolio value
portfolio_returns <- c()  # to record the weekly returns
while (week_end <= end_date) {
  # Buy the call spread
  call_strike <- round(get_option_price(tqqq_ticker, week_end), 1)
  call_buy_price <- round(call_strike - call_spread_width/2, 2)
  call_sell_price <- round(call_strike + call_spread_width/2, 2)
  cat("Week starting", week_start, " - buying TQQQ call spread with strikes", call_buy_price, "and", call_sell_price, "\n")
  
  # Buy the put spread
  put_strike <- round(get_option_price(sqqq_ticker, week_end), 1)
  put_buy_price <- round(put_strike + put_spread_width/2, 2)
  put_sell_price <- round(put_strike - put_spread_width/2, 2)
  cat("Week starting", week_start, " - buying SQQQ put spread with strikes", put_buy_price, "and", put_sell_price, "\n")
  
  # Close the positions on the last working day of the week
  week_last_working_day <- week_end - as.numeric(format(week_end, "%w")) + 4
  if (week_last_working_day == week_end) {
    week_last_working_day <- week_end - 1
  }
  call_profit <- round(get_option_price(tqqq_ticker, week_last_working_day) - call_strike, 2)
  put_profit <- round(put_strike - get_option_price(sqqq_ticker, week_last_working_day), 2)
  week_return <- call_profit + put_profit
  portfolio_value <- portfolio_value + week_return
  portfolio_returns <- c(portfolio_returns, week_return)
  cat("Week ending", week_end, " - closing positions with call profit", call_profit, "and put profit", put_profit, "\n")
  cat("Portfolio value:", portfolio_value, "\n\n")
  
  # Move to the next week
  week_start <- week_end + 1
  week_end <- week_start + 6
}

# Print the final portfolio value and returns
cat("Final portfolio value:", portfolio_value, "\n")
cat("Weekly returns:", paste0)
