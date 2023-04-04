library(quantmod)

getOptionChain <- function(symbol, expDate) {
  # Get the option chain data for the specified symbol and expiration date
  opt_chain <- getOptionChain(symbol, expDate)
  
  # Extract the call and put data from the option chain
  calls <- opt_chain$calls
  puts <- opt_chain$puts
  
  # Format the data as a data frame
  data <- data.frame(
    Strike = c(calls$Strike, puts$Strike),
    Expiry = c(rep(expDate, nrow(calls)), rep(expDate, nrow(puts))),
    Type = c(rep("Call", nrow(calls)), rep("Put", nrow(puts))),
    Last = c(calls$Last, puts$Last),
    Bid = c(calls$Bid, puts$Bid),
    Ask = c(calls$Ask, puts$Ask),
    Volume = c(calls$Volume, puts$Volume),
    OpenInterest = c(calls$OpenInterest, puts$OpenInterest)
  )
  
  return(data)
}
