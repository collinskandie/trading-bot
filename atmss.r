library(quantmod)
library(dplyr)
library(tidyr)

# Set variables for call and put spread width, start and end date, and other parameters # nolint
call_spread_width <- 0.05
put_spread_width <- 0.05
start_date <- "2022-01-01" # nolint
end_date <- "2022-12-31" # nolint

# Define a function to calculate the mid-market rate for the call and put options # nolint
mid_market_rate <- function(bid, ask) {
    (bid + ask) / 2
}
record_trade <- function(portfolio, symbol, quantity, price) {
    if (symbol == "TQQQ") {
        portfolio$tqqq <- portfolio$tqqq + quantity
    } else if (symbol == "SQQQ") {
        portfolio$sqqq <- portfolio$sqqq + quantity
    }
    portfolio$cash <- portfolio$cash - quantity * price
    return(portfolio)
}

# Usage
portfolio <- list(cash = 100000, tqqq = 0, sqqq = 0)
# portfolio <- record_trade(portfolio, "TQQQ", 10, 100) # nolint
# Append the trade to the portfolio data frame
trade <- data.frame(Date = date, Symbol = symbol, Action = action, Quantity = quantity, Price = price, Profit_Loss = profit_loss, stringsAsFactors = FALSE) # nolint: line_length_linter.
# portfolio <<- rbind(portfolio, trade) # nolint
portfolio <- record_trade(portfolio, trade)
# Print a message
message(paste0(action, " ", abs(quantity), " shares of ", symbol, " at $", price, " per share on ", date, ".")) # nolint # }
# Define a function to execute the trade
execute_trade <- function() { # nolint
    # Load TQQQ and SQQQ data from Yahoo Finance
    message("Loading TQQQ and SQQQ data from Yahoo Finance...")
    tqqq <- getSymbols("TQQQ", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) # nolint
    sqqq <- getSymbols("SQQQ", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) # nolint
     # Display message
    message("TQQQ and SQQQ data loaded from Yahoo Finance.")
    # Calculate the ATM strike price for TQQQ and SQQQ
    tqqq_atm <- round(tail(Cl(tqqq), 1) / call_spread_width) * call_spread_width
    sqqq_atm <- round(tail(Cl(sqqq), 1) / put_spread_width) * put_spread_width
    # Calculate the call and put strike prices for the spread
    call_strike <- tqqq_atm + call_spread_width / 2
    put_strike <- sqqq_atm - put_spread_width / 2
    # Get the call and put options data from Yahoo Finance
    call_options <- getOptionChain("TQQQ", NULL, from = start_date, to = end_date)$c # nolint: line_length_linter.
    put_options <- getOptionChain("SQQQ", NULL, from = start_date, to = end_date)$p # nolint
    # Filter the call and put options data to only include the ATM options for the desired expiration date # nolint
    call_atm <- call_options %>% filter(strike == call_strike & type == "call") # nolint
    put_atm <- put_options %>% filter(strike == put_strike & type == "put") # nolint
    # Calculate the mid-market rate for the call and put options
    call_mid <- mid_market_rate(call_atm$Bid, call_atm$Ask)
    put_mid <- mid_market_rate(put_atm$Bid, put_atm$Ask)
    # Buy the call and put spread at the mid-market rate
    call_buy_price <- call_mid[1] * 100
    call_sell_price <- call_mid[2] * 100
    put_buy_price <- put_mid[1] * 100
    put_sell_price <- put_mid[2] * 100
    # Calculate the profit or loss from the sale
    call_profit <- (call_sell_price - call_buy_price) * 100
    put_profit <- (put_sell_price - put_buy_price) * 100
    total_profit <- call_profit + put_profit
    Type = c("call", "put"), Strike = c(call_strike, put_strike), # nolint
    record_trade(Date = start_date, Ticker = c("TQQQ", "SQQQ"), Type = c("call", "put"), Strike = c(call_strike, put_strike), Buy_Price = c(call_buy_price, put_buy_price), Profit_Loss = c(call_profit, put_profit), Profit_Loss = c(call_profit, put_profit), # nolint
        stringsAsFactors = FALSE
    ) # nolint: line_length_linter.
    portfolio <- data.frame(
        Date = start_date, Ticker = c("TQQQ", "SQQQ"),
        Type = c("call", "put"), Strike = c(call_strike, put_strike),
        Buy_Price = c(call_buy_price, put_buy_price),
        Sell_Price = c(call_sell_price, put_sell_price),
        Profit_Loss = c(call_profit, put_profit),
        stringsAsFactors = FALSE)
    # Print the portfolio and total profit/loss
    print(portfolio)
    print(paste0("Total Profit/Loss: $", total_profit))
    # Return the portfolio and total profit/loss
    return(list(portfolio = portfolio, total_profit = total_profit))
}
# Execute the trade on Fridays
if (weekdays(Sys.Date()) == "Friday") {
    portfolio <- execute_trade()
}
# Group the portfolio by week and year
portfolio %>%
    # group_by(week = format(Date, "%U-%Y")) %>%
summarize(total_profit = sum(Profit_Loss))
