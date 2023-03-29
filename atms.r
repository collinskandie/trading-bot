library(quantmod)

# Define the TQQQ and SQQQ symbols
tqqq_symbol <- "TQQQ"
sqqq_symbol <- "SQQQ"

# Define the width of the spreads (as a percentage of the underlying price)
spread_width <- 0.05

# Define the function to calculate the ATM strike price
calc_atm_strike <- function(symbol) {
  # Get the current price of the underlying
  price <- Cl(getSymbols(symbol, auto.assign = FALSE))
  # Get the options chain for the next monthly expiration
  options_chain <- getOptionChain(symbol, NULL, NULL, NULL, "next")
  # Calculate the ATM strike price
  atm_strike <- options_chain$puts$Strike[which.min(abs(options_chain$puts$Strike - price))]
  return(atm_strike)
}

# Define the function to buy the call spread
buy_call_spread <- function(symbol, spread_width) {
  # Calculate the ATM strike price
  atm_strike <- calc_atm_strike(symbol)
  # Calculate the lower and upper strike prices for the spread
  lower_strike <- atm_strike * (1 - spread_width)
  upper_strike <- atm_strike * (1 + spread_width)
  # Buy the spread at the mid-market rate
  options_chain <- getOptionChain(symbol, NULL, NULL, NULL, "next")
  call_bid <- options_chain$calls$Bid[which.min(abs(options_chain$calls$Strike - lower_strike))]
  call_ask <- options_chain$calls$Ask[which.min(abs(options_chain$calls$Strike - upper_strike))]
  call_mid <- (call_bid + call_ask) / 2
  buy_call <- data.frame(symbol = symbol, option_type = "call", strike_price = c(lower_strike, upper_strike), 
                          price = c(call_mid, call_mid), quantity = c(-1, 1))
  return(buy_call)
}

# Define the function to buy the put spread
buy_put_spread <- function(symbol, spread_width) { 
  # Calculate the ATM strike price
  atm_strike <- calc_atm_strike(symbol)
  # Calculate the lower and upper strike prices for the spread
  lower_strike <- atm_strike * (1 - spread_width)
  upper_strike <- atm_strike * (1 + spread_width)
  # Buy the spread at the mid-market rate
  options_chain <- getOptionChain(symbol, NULL, NULL, NULL, "next")
  put_bid <- options_chain$puts$Bid[which.min(abs(options_chain$puts$Strike - upper_strike))]
  put_ask <- options_chain$puts$Ask[which.min(abs(options_chain$puts$Strike - lower_strike))]
  put_mid <- (put_bid + put_ask) / 2
  buy_put <- data.frame(symbol = symbol, option_type = "put", strike_price = c(lower_strike, upper_strike), 
                         price = c(put_mid, put_mid), quantity = c(-1, 1))
  return(buy_put)
}

# Define the function to close a position
close_position <- function(position) {
  # Calculate the profit/loss on the position
  pnl <- sum(position$price * position$quantity)
  # Remove the position from the portfolio
  portfolio <<- portfolio[!duplicated(portfolio),]
  # Add the profit/loss to the total P&L
  total_pnl <<- total_pnl + pnl
  # Print the P&L for the position
  cat("Closed position in", position$symbol, "with P&L of", pnl, "\n")
}
