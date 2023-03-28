import yfinance as yf
import datetime as dt
import numpy as np
import pandas as pd
import time

# Set up the trading parameters
tqqq_ticker = 'TQQQ'  # The ticker symbol for TQQQ
sqqq_ticker = 'SQQQ'  # The ticker symbol for SQQQ
spread_pct = 0.05     # The percentage width of the spreads
portfolio = []        # A list to hold the portfolio transactions
weekly_pnl = []       # A list to hold the weekly P&L data

# Define a function to get the mid-market price of an option
def get_mid_price(symbol, expiration, option_type, strike):
    option_chain = yf.Ticker(symbol).option_chain(expiration)
    option_data = option_chain.options[option_chain.options.index[0]]
    option = option_chain.get_call_data(strike) if option_type == 'C' else option_chain.get_put_data(strike)
    bid_price = option['bid'][0]
    ask_price = option['ask'][0]
    return (bid_price + ask_price) / 2

# Define a function to calculate the P&L of an options trade
def calculate_pnl(symbol, entry_price, exit_price):
    multiplier = 100   # The number of shares represented by one options contract
    commission = 1.0   # The commission per options contract
    pnl = (exit_price - entry_price - commission) * multiplier
    portfolio.append((symbol, pnl))
    return pnl

# Define a function to close out an options position
def close_position(symbol, expiration, option_type, spread_width):
    option_chain = yf.Ticker(symbol).option_chain(expiration)
    option_data = option_chain.options[option_chain.options.index[0]]
    strikes = option_chain.strikes
    atm_strike = strikes[np.argmin(np.abs(strikes - option_data['lastPrice']))]
    long_strike = atm_strike - spread_width / 2
    short_strike = atm_strike + spread_width / 2
    long_price = get_mid_price(symbol, expiration, option_type, long_strike)
    short_price = get_mid_price(symbol, expiration, option_type, short_strike)
    exit_price = (long_price - short_price) * 100
    return calculate_pnl(symbol, portfolio[-1][1], exit_price)

