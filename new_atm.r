library(quantmod)
library(dplyr)
library(tidyr)

# Set variables for call and put spread width, start and end date, and other parameters
call_spread_width <- 0.05
put_spread_width <- 0.05
start_date <- "2022-01-01"
end_date <- "2022-12-31"

# Define a function to calculate the mid-market rate for the call and put options
mid_market_rate <- function(bid, ask) {
    (bid + ask) / 2
}

record_trade <- function(portfolio, trade) {
    if (trade$Ticker == "TQQQ") {
        portfolio$tqqq <- portfolio$tqqq + trade$Quantity
    } else if (trade$Ticker == "SQQQ") {
        portfolio$sqqq <- portfolio$sqqq + trade$Quantity
    }
    portfolio$cash <- portfolio$cash - trade$Quantity * trade$Price
    return(portfolio)
}

# Usage
portfolio <- list(cash = 100000, tqqq = 0, sqqq = 0)

# Define a function to execute the trade
execute_trade <- function(portfolio) {
  
  # Load TQQQ and SQQQ data from Yahoo Finance
  message("Loading TQQQ and SQQQ data from Yahoo Finance...")
  tqqq <- getSymbols("TQQQ", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  sqqq <- getSymbols("SQQQ", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  message("TQQQ and SQQQ data loaded from Yahoo Finance.")
  
  # Calculate the ATM strike price for TQQQ and SQQQ
  tqqq_atm <- round(tail(Cl(tqqq), 1) / call_spread_width) * call_spread_width
  sqqq_atm <- round(tail(Cl(sqqq), 1) / put_spread_width) * put_spread_width
  
  # Calculate the call and put strike prices for the spread
  call_strike <- tqqq_atm + call_spread_width / 2
  put_strike <- sqqq_atm - put_spread_width / 2
  
  # Get the call and put options data from Yahoo Finance
  call_options <- getOptionChain("TQQQ", NULL, from = start_date, to = end_date, strike = call_strike, auto.assign = FALSE)
  put_options <- getOptionChain("SQQQ", NULL, from = start_date, to = end_date, strike = put_strike, auto.assign = FALSE)
  
  # Calculate the bid and ask prices for the call and put options
  call_bid <- as.numeric(call_options[,2])
  call_ask <- as.numeric(call_options[,3])
  put_bid <- as.numeric(put_options[,2])
  put_ask <- as.numeric(put_options[,3])
  
  # Calculate the midpoint of the bid-ask spread
  call_mid <- (call_bid + call_ask) / 2
  put_mid <- (put_bid + put_ask) / 2
  
  # Calculate the net debit for the trade
  net_debit <- call_mid + put_mid
  
  # Calculate the number of contracts to trade
  contracts_to_trade <- round(portfolio / net_debit)
  
  # Buy the call and put options
  order <- paste0(contracts_to_trade, "C:TQQQ", format(call_strike, digits = 4), " ", 
                  contracts_to_trade, "P:SQQQ", format(put_strike, digits = 4))
  message("Buying options with order: ", order)
  trade <- placeOrder(account, contract = twsContract(order), order = twsOrder(net_debit * contracts_to_trade, "MKT", "BUY"))
  message("Trade executed successfully.")
}
execute_trade(portfolio)