from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from pandas_datareader import data as pdr
import pandas as pd

# establish a connection to the TWS trading platform
class IBapi(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
    
    def error(self, reqId, errorCode, errorString):
        print("Error: ", reqId, " ", errorCode, " ", errorString)

tws = IBapi()
tws.connect('127.0.0.1', 7497, 123)

# Set variables for call and put spread width, start and end date, and other parameters
call_spread_width = 0.05
put_spread_width = 0.05
start_date = "2022-01-01"
end_date = "2022-12-31"

# Define a function to calculate the mid-market rate for the call and put options
def mid_market_rate(bid, ask):
    return (bid + ask) / 2

def record_trade(portfolio, trade):
    if trade['Ticker'] == "TQQQ":
        portfolio['tqqq'] += trade['Quantity']
    elif trade['Ticker'] == "SQQQ":
        portfolio['sqqq'] += trade['Quantity']
    portfolio['cash'] -= trade['Quantity'] * trade['Price']
    return portfolio

# Usage
portfolio = {'cash': 100000, 'tqqq': 0, 'sqqq': 0}

# Define a function to execute the trade
def execute_trade(portfolio):
    # Load TQQQ and SQQQ data from Yahoo Finance
    tqqq = pdr.get_data_yahoo('TQQQ', start_date, end_date)
    sqqq = pdr.get_data_yahoo('SQQQ', start_date, end_date)

    # Calculate the ATM strike price for TQQQ and SQQQ
    tqqq_atm = round(tqqq['Close'].iloc[-1] / call_spread_width) * call_spread_width
    sqqq_atm = round(sqqq['Close'].iloc[-1] / put_spread_width) * put_spread_width
    
    # Calculate the call and put strike prices for the spread
    call_strike = tqqq_atm + call_spread_width / 2
    put_strike = sqqq_atm - put_spread_width / 2
    
    # Get the call and put options data from Yahoo Finance
    call_options = pd.DataFrame()
    put_options = pd.DataFrame()

    # Extract relevant columns and convert to numeric
    call_bid = pd.to_numeric(call_options['TQQQ Bid'])
    call_ask = pd.to_numeric(call_options['TQQQ Ask'])
    put_bid = pd.to_numeric(put_options['SQQQ Bid'])
    put_ask = pd.to_numeric(put_options['SQQQ Ask'])

    # Calculate the midpoint of the bid-ask spread
    call_mid = mid_market_rate(call_bid, call_ask)
    put_mid = mid_market_rate(put_bid, put_ask)
    # Calculate the premium received and margin required for the call and put spreads
call_premium_received = call_mid - (tqqq_atm - call_spread_width / 2)
put_premium_received = put_mid - (sqqq_atm + put_spread_width / 2)

call_margin_required = (tqqq_atm - call_strike) * 100
put_margin_required = (put_strike - sqqq_atm) * 100

# Execute the trade if there is enough cash in the portfolio to cover the margin required
if portfolio['cash'] >= call_margin_required + put_margin_required:
    call_trade = {'Ticker': 'TQQQ', 'Quantity': -100, 'Price': call_mid}
    put_trade = {'Ticker': 'SQQQ', 'Quantity': 100, 'Price': put_mid}
    portfolio = record_trade(portfolio, call_trade)
    portfolio = record_trade(portfolio, put_trade)
    
print(portfolio)

