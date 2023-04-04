library(quantmod)
library(dplyr)
library(tidyr)
library(IBrokers) # library containing placeOrder Api.

# establish a connection to the TWS trading platform
tws <- twsConnect()
# Set variables for call and put spread width, start and end date, and other parameters # nolint
call_spread_width <- 0.05
put_spread_width <- 0.05
start_date <- "2022-01-01"
end_date <- "2022-12-31"
# Define a function to calculate the mid-market rate for the call and put options # nolint
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
    tqqq <- getSymbols("TQQQ", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) # nolint
    sqqq <- getSymbols("SQQQ", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) # nolint
    message("TQQQ and SQQQ data loaded from Yahoo Finance.")
    # Calculate the ATM strike price for TQQQ and SQQQ
    tqqq_atm <- round(tail(Cl(tqqq), 1) / call_spread_width) * call_spread_width
    sqqq_atm <- round(tail(Cl(sqqq), 1) / put_spread_width) * put_spread_width
    # Calculate the call and put strike prices for the spread
    call_strike <- tqqq_atm + call_spread_width / 2
    put_strike <- sqqq_atm - put_spread_width / 2
    # Get the call and put options data from Yahoo Finance
    call_options <- getOptionChain("TQQQ", NULL, from = start_date, to = end_date, strike = call_strike, auto.assign = FALSE) # nolint
    put_options <- getOptionChain("SQQQ", NULL, from = start_date, to = end_date, strike = put_strike, auto.assign = FALSE) # nolint

    # Extract relevant columns and convert to numeric
    call_bid <- as.numeric(call_options$TQQQ.Bid)
    call_ask <- as.numeric(call_options$TQQQ.Ask)
    put_bid <- as.numeric(put_options$SQQQ.Bid)
    put_ask <- as.numeric(put_options$SQQQ.Ask)
    # Calculate the midpoint of the bid-ask spread
    call_mid <- (call_bid + call_ask) / 2
    put_mid <- (put_bid + put_ask) / 2
    # Calculate the net debit for the trade
    net_debit <- call_mid + put_mid
    # Calculate the number of contracts to trade
    contracts_to_trade <- floor(portfolio$cash / net_debit)
    # Buy the call and put options
    order <- paste0(
        contracts_to_trade, "C:TQQQ", format(call_strike, digits = 4), " ", # nolint
        contracts_to_trade, "P:SQQQ", format(put_strike, digits = 4)
    )
    message("Buying options with order: ", order)
    # this part requires you to install tws desktop app. the set the port to default port=7496 #nolint
    # Note that in order to use TWS, you will need to have a funded account with Interactive Brokers or a demo account to test the functionality. #nolint
    # Parse the order string and extract the relevant information
    message(order)
    order_parts <- strsplit(order, split = " ")[[1]]
    call_contract_parts <- strsplit(order_parts[1], split = ":")[[1]]
    put_contract_parts <- strsplit(order_parts[2], split = ":")[[1]]
    message(call_contract_parts)
    message(put_contract_parts)

    # Create the contract and order objects using the extracted information
    # call_contract <- twsContract(
    #     conId = 1234, symbol = "TQQQ", sectype = "STK", exch = "SMART",
    #     currency = "USD", primary = TRUE, multiplier = "",
    #     expiry = "20230618", strike = 150, right = "C", local = "",
    #     combo_legs_desc = "", comboleg = "", include_expired = FALSE,
    #     secIdType = "", secId = "", tradingClass = ""
    # )
    # put_contract <- twsContract(
    #     conId = 1234, symbol = "SQQQ", sectype = "STK", exch = "SMART",
    #     currency = "USD", primary = TRUE, multiplier = "",
    #     expiry = "20230618", strike = 150, right = "C", local = "",
    #     combo_legs_desc = "", comboleg = "", include_expired = FALSE,
    #     secIdType = "", secId = "", tradingClass = ""
    # )
    order_obj <- twsOrder(
        action = "BUY", totalQuantity = 100,
        orderType = "LMT", lmtPrice = 140
    )
    # Create the contract and order objects using the extracted information
    call_contract <- twsContract(
        conId = as.numeric(call_contract_parts[1]),
        symbol = call_contract_parts[2],
        sectype = "STK",
        exch = call_contract_parts[4],
        currency = call_contract_parts[5],
        primary = TRUE, 
        multiplier = call_contract_parts[6],
        expiry = call_contract_parts[7],
        strike = format(call_strike, digits = 2),
        right = call_contract_parts[9],
        local = TRUE,
        combo_legs_desc = call_contract_parts[11],
        comboleg = call_contract_parts[12],
        include_expired = FALSE,
        secIdType = call_contract_parts[13],
        secId = call_contract_parts[14],
        tradingClass = call_contract_parts[15]
    )
    put_contract <- twsContract(
        conId = as.numeric(put_contract_parts[1]),
        symbol = put_contract_parts[2],
        sectype =  "STK",
        exch = put_contract_parts[4],
        currency = put_contract_parts[5],
        primary = TRUE, multiplier = put_contract_parts[6],
        expiry = put_contract_parts[7],
        strike = format(put_strike, digits = 2),
        right = put_contract_parts[9],
        local = TRUE,
        combo_legs_desc = put_contract_parts[11],
        comboleg = put_contract_parts[12],
        include_expired = FALSE,
        secIdType = put_contract_parts[13],
        secId = put_contract_parts[14],
        tradingClass = put_contract_parts[15]
    )

    # Place the orders using the contracts and order objects
    trade_call <- reqMktData(tws, call_contract)
    placeOrder(
        # change account
        accountId = "", #user your iBrooker userId
        contract = call_contract, order = order_obj
    )

    trade_put <- reqMktData(tws, put_contract)
    placeOrder(
        # change account
        accountId = "", #user your iBrooker userId
        contract = put_contract, order = order_obj
    )
    print(trade_call)
    print(trade_put)


    # placeOrder(tws, twsOrder(net_debit * contracts_to_trade, "MKT", "BUY")) #nolint
    message("Trade executed successfully.")
    # Calculate the profit and loss for the call and put options
    call_pnl <- (call_bid - call_mid) * contracts_to_trade * 100
    put_pnl <- (put_bid - put_mid) * contracts_to_trade * 100

    # Update the portfolio with the profit and loss
    portfolio$cash <- portfolio$cash - net_debit * contracts_to_trade
    portfolio$tqqq <- portfolio$tqqq + contracts_to_trade
    portfolio$sqqq <- portfolio$sqqq + contracts_to_trade
    portfolio$pnl <- portfolio$pnl + call_pnl + put_pnl
    # print portfolio
    print(portfolio)

    return(portfolio)
}
for (date in seq(as.Date(start_date), as.Date(end_date), by = "day")) {
    date <- as.Date(date)
    if (weekdays(date) == "Friday") {
        execute_trade(portfolio)
        print(portfolio)
    } else if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        # Do nothing on weekends
        next
    } else {
        # Check if options expiration or last working day of week
        if (format(as.Date(date), "%Y%m") != format(
            (as.Date(date) + 7),
            "%Y%m"
        ) | weekdays(as.Date(date) + 7) == "Saturday") {
            # Close position and update portfolio
            currentWeek <- portfolio[nrow(portfolio), ]
            tqqq_call_profit_loss <- currentWeek$tqqq_call_sell -
                getOptionPrice(tqqqTicker, as.Date(date))
            sqqq_put_profit_loss <- getOptionPrice(
                sqqqTicker,
                as.Date(date)
            ) - currentWeek$sqqq_put_sell
            profit_loss <- tqqq_call_profit_loss + sqqq_put_profit_loss
        }
    }
}
