library(quantmod)
library(RQuantLib)

# Define the symbol and expiry date
symbol <- "SQQQ"
expiry <- as.Date("2023-03-31")

# Get the last available option chain for the symbol
options <- getOptionChain(symbol)

# Extract the at the money call option for the expiry date
atm_call <- options$ca[options$ca$moneyness == "ATMF" & options$ca$expiry == expiry, ]

# Define the strike price and option quantity
strike <- atm_call$strike
quantity <- 100 # 1 option contract is for 100 shares

# Define the Friday closing time
friday_close <- as.POSIXct(paste(Sys.Date(), "16:00:00"), tz = "America/New_York")

# Define the expiration day closing time
expiration_close <- as.POSIXct(paste(expiry - 1, "16:00:00"), tz = "America/New_York")

# Sell the at the money call option every Friday
while (TRUE) {
  current_time <- Sys.time()
  if (current_time > friday_close) {
    # Sell the option
    order <- order_book(portfolio = "my_portfolio",
                         symbol = symbol,
                         quantity = -quantity,
                         type = "option",
                         strike = strike,
                         expiry = expiry,
                         side = "sell",
                         order_type = "mkt")
    # Exit the loop until next Friday
    break
  }
  # Wait for 1 minute before checking the time again
  Sys.sleep(60)
}

# Close the position a day before expiration
while (TRUE) {
  current_time <- Sys.time()
  if (current_time > expiration_close) {
    # Buy back the option
    order <- order_book(portfolio = "my_portfolio",
                         symbol = symbol,
                         quantity = quantity,
                         type = "option",
                         strike = strike,
                         expiry = expiry,
                         side = "buy",
                         order_type = "mkt")
    # Exit the loop
    break
  }
  # Wait for 1 minute before checking the time again
  Sys.sleep(60)
}