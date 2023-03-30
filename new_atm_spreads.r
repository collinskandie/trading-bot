# load the required packages.
library(quantmod)
library(tidyverse)

# remove conflict if dplyr and lag are installed in R env

conflictRules("dplyr", exclude = "lag")

# Define the ticker symbols for TQQQ and SQQQ
tqqq <- "TQQQ"
sqqq <- "SQQQ"

# Define the width of the spreads and the mid-market rate adjustment
spread_width <- 0.05
mid_market_adjustment <- 0

# Define the start and end dates (preferably past dates 2022)
# this dates can be changed
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")

# Define the function to calculate the mid-market rate
mid_market_rate <- function(x, y) {
    (x + y) / 2 + mid_market_adjustment
}

# Define the function to calculate the weekly portfolio return
weekly_return <- function(portfolio) {
    diff(log(portfolio))[length(portfolio) - 1]
}

# Create an empty portfolio to record the profits and losses
portfolio <- c()

# Loop through each Friday in the date range
for (date in seq(start_date, end_date, by = "day")) {
    # Check if the current day is a Friday
    if (weekdays(as.Date(date)) == "Friday") {
        # Get the current TQQQ and SQQQ prices
        tqqq_price <- as.numeric(getQuote(tqqq, date)$Last)
        sqqq_price <- as.numeric(getQuote(sqqq, date)$Last)
        # Calculate the strike prices for the call and put spreads
        call_strike <- round(tqqq_price * (1 + spread_width), 2)
        put_strike <- round(sqqq_price * (1 - spread_width), 2)
        # Calculate the mid-market rates for the call and put spreads
        call_mid <- (getOptionQuote(paste0(tqqq, call_strike), date)$bid + getOptionQuote(paste0(tqqq, call_strike), date)$ask) / 2 # nolint
        put_mid <- (getOptionQuote(paste0(sqqq, put_strike), date)$bid + getOptionQuote(paste0(sqqq, put_strike), date)$ask) / 2 # nolint

        # call_mid <- mid_market_rate(getQuote(paste0(tqqq, call_strike), date)$Bid, getQuote(paste0(tqqq, call_strike), date)$Ask) # nolint
        # put_mid <- mid_market_rate(getQuote(paste0(sqqq, put_strike), date)$Bid, getQuote(paste0(sqqq, put_strike), date)$Ask) # nolint
        # Record the profits and losses from the previous position
        if (length(portfolio) > 0) {
            portfolio <- c(portfolio, portfolio[length(portfolio)] + (call_mid - portfolio[length(portfolio) - 1]) - (put_mid - portfolio[length(portfolio) - 1])) # nolint
        }
        # Buy the call and put spreads at the mid-market rate
        portfolio <- c(portfolio, call_mid, -put_mid)
        # Check if the options expiration date or the last working day of the week is approaching # nolint
        # Check if the options expiration date or the last working day of the week is approaching # nolint
        if ((as.numeric(format(date, "%u")) >= 5 & as.numeric(format(date, "%u")) <= 7) | as.Date(paste0(format(date, "%Y-%m-"), as.numeric(format(date, "%d")) + 4)) %in% getOptionChain(tqqq)@expirations) { # nolint
            # Calculate the profits and losses from the current position
            portfolio <- c(portfolio, call_mid - quantmod::getQuote(tqqq, call_strike, date)$Last, -put_mid + quantmod::getQuote(sqqq, put_strike,date)$Last) # nolint
            # Close the position
            portfolio <- c(portfolio, 0, 0)
        }
        # Print the current portfolio and weekly return
        print(paste0("Portfolio: ", portfolio))
        print(paste0("Weekly Return: ", weekly_return(portfolio)))
    }
}
