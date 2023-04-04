# Load required packages
library(quantmod)
library(tidyverse)

conflictRules('dplyr', exclude = 'lag')
# Define the ticker symbols and option expiration date
tqqq <- "TQQQ"
sqqq <- "SQQQ"
opt_exp <- "monthly"

# Define the option spread width as a percentage
spread_pct <- 5

# Define a function to get the mid-market rate for an option spread
get_mid_market_rate <- function(spread) {
  # Get the bid and ask prices for the spread
  bid <- spread$bid
  ask <- spread$ask
  
  # Calculate the mid-market rate
  mid_market_rate <- (bid + ask) / 2
  
  # Return the mid-market rate
  mid_market_rate
}

# Define a function to close the option spread position
close_position <- function(pos) {
  # Get the current date and time
  now <- Sys.time()
  
  # Get the expiration date for the options position
  exp_date <- as.Date(pos$expiration)
  
  # Check if the expiration date is equal to or before the current date
  if (exp_date <= now) {
    # Close the options position
    close_order <- placeOrder(tqqq, "MKT", -pos$tqqq_pos)
    close_order <- placeOrder(sqqq, "MKT", -pos$sqqq_pos)
    
    # Calculate the profit or loss from the options position
    tqqq_profit_loss <- pos$tqqq_pos * (pos$tqqq_sell_price - pos$tqqq_buy_price)
    sqqq_profit_loss <- pos$sqqq_pos * (pos$sqqq_sell_price - pos$sqqq_buy_price)
    profit_loss <- tqqq_profit_loss + sqqq_profit_loss
    
    # Add the profit or loss to the portfolio
    portfolio$balance <- portfolio$balance + profit_loss
    
    # Print a message about the closed position and its profit or loss
    message(paste("Closed position on", exp_date, "with a profit or loss of", round(profit_loss, 2)))
  } else {
    # Print a message that the position is still open
    message("Position still open")
  }
}

# Define a function to update the portfolio
update_portfolio <- function() {
  # Calculate the current portfolio balance
  balance <- portfolio$balance
  
  # Calculate the weekly return
  if (nrow(portfolio$log) >= 2) {
    prev_balance <- portfolio$log$balance[nrow(portfolio$log) - 1]
    weekly_return <- (balance - prev_balance) / prev_balance
  } else {
    weekly_return <- 0
  }
  
  # Add the current balance and weekly return to the log
  portfolio$log <- portfolio$log %>% 
    add_row(date = Sys.Date(), balance = balance, weekly_return = weekly_return)
  
  # Print the current portfolio and weekly return
  message(paste("Current portfolio: $", round(balance, 2)))
  message(paste("Weekly return:", round(100 * weekly_return, 2), "%"))
}

# Set the initial portfolio balance to $100,000
portfolio <- list(balance = 100000, log = data.frame(date = as.Date(character()), balance = numeric(), weekly_return = numeric()))

# Define a function to execute the trading strategy every Friday
# execute_strategy <- function() {
#   # Check if today is Friday
#   if (weekdays(Sys.Date()) == "Friday") {
#     # Get the current price of TQQQ and SQQQ
#     tqqq_price <- Cl(getSymbols
# Define a function to execute the trading strategy every Friday
execute_strategy <- function() {
  # Check if today is Friday
  if (weekdays(Sys.Date()) == "Friday") {
    # Get the current price of TQQQ and SQQQ
    tqqq_price <- Cl(getSymbols(tqqq, auto.assign = FALSE))
    sqqq_price <- Cl(getSymbols(sqqq, auto.assign = FALSE))
    
    # Get the options expiration date for the next month
    exp_date <- getNextOptionExpiry(tqqq, type = opt_exp)[[1]]
    
    # Define the strike prices for the call and put spreads
    call_strike <- round(tqqq_price * (1 + spread_pct / 100), 2)
    put_strike <- round(sqqq_price * (1 - spread_pct / 100), 2)
    
    # Get the call and put spreads for TQQQ and SQQQ at the mid-market rate
    tqqq_call_spread <- getOptionSpread(tqqq, opttype = "call", 
                                        expiration = exp_date, 
                                        s = call_strike, 
                                        s2 = call_strike - spread_pct / 100 * tqqq_price)
    tqqq_call_mid_market_rate <- get_mid_market_rate(tqqq_call_spread)
    
    sqqq_put_spread <- getOptionSpread(sqqq, opttype = "put", 
                                        expiration = exp_date, 
                                        s = put_strike, 
                                        s2 = put_strike + spread_pct / 100 * sqqq_price)
    sqqq_put_mid_market_rate <- get_mid_market_rate(sqqq_put_spread)
    
    # Place the orders for the call and put spreads at the mid-market rate
    tqqq_call_order <- placeOrder(tqqq, "LMT", tqqq_call_spread$n,
                                  price = tqqq_call_mid_market_rate, 
                                  transmit = FALSE)
    sqqq_put_order <- placeOrder(sqqq, "LMT", sqqq_put_spread$n,
                                  price = sqqq_put_mid_market_rate, 
                                  transmit = FALSE)
    
    # Get the fill prices for the call and put spreads
    tqqq_call_fill <- getExecutions(tqqq_call_order$id)
    sqqq_put_fill <- getExecutions(sqqq_put_order$id)
    
    # Update the portfolio with the call and put spread positions
    portfolio$tqqq_pos <- tqqq_call_fill$qty
    portfolio$tqqq_buy_price <- tqqq_call_fill$price
    portfolio$tqqq_sell_price <- tqqq_call_fill$price - spread_pct / 100 * tqqq_price
    
    portfolio$sqqq_pos <- sqqq_put_fill$qty
    portfolio$sqqq_buy_price <- sqqq_put_fill$price
    portfolio$sqqq_sell_price <- sqqq_put_fill$price + spread_pct / 100 * sqqq_price
    
    portfolio$balance <- portfolio$balance - tqqq_call_fill$value - sqqq_put_fill$value
    
    # Print a message about the new positions
    message(paste("Opened new positions on", exp_date))
    # Close the option spread positions on the expiration date or last working day of the week
    close_position(portfolio)
    
    # Update the portfolio
    update_portfolio()
  }
}

# Execute the trading strategy every minute
while (TRUE) {
  execute_strategy()
  Sys.sleep(60)
}

