import datetime
import yfinance as yf
import numpy as np
import pandas as pd
from pandas.tseries.offsets import BDay
from yfinance.utils import get_json

# Define constants
TICKER_TQQQ = "TQQQ"
TICKER_SQQQ = "SQQQ"
STRATEGY_NAME = "ATM Monthly Call Spreads in TQQQ and ATM Monthly Put Spreads in SQQQ"
STRATEGY_START_DATE = "2022-01-01"
STRATEGY_END_DATE = "2022-12-31"
SPREAD_WIDTH = 0.05  # 5%
CLOSE_THRESHOLD = 0.2  # 20%
PORTFOLIO = pd.DataFrame(columns=["Date", "Position", "P/L"])
DATE_FORMAT = "%Y-%m-%d"
# Define function to get the mid-market rate for a given option
def get_mid_market_rate(option):
    bid = option.get("bid", 0)
    ask = option.get("ask", 0)
    return (bid + ask) / 2
# Define function to buy a call spread and a put spread at the mid-market rate
def buy_spreads(date, tqqq_strike, sqqq_strike):
    tqqq = yf.Ticker(TICKER_TQQQ)
    sqqq = yf.Ticker(TICKER_SQQQ)
    expiration_dates = tqqq.options

    # Find the expiration date that is closest to one month from now
    exp_date = min(expiration_dates, key=lambda d: abs((datetime.datetime.strptime(d, '%Y-%m-%d') - date).days - 30))
    tqqq_calls = tqqq.option_chain(exp_date).calls
    sqqq_puts = sqqq.option_chain(exp_date).puts

    # Find the call option with the closest strike price to the TQQQ price
    tqqq_option = min(tqqq_calls, key=lambda option: abs(float(option.get("strike", 0)) - tqqq_strike))

    # Find the put option with the closest strike price to the SQQQ price
    sqqq_option = min(sqqq_puts, key=lambda option: abs(float(option.get("strike", 0)) - sqqq_strike))

    # Buy the call spread and the put spread at the mid-market rate
    tqqq_call_price = get_mid_market_rate(tqqq_option)
    tqqq_put_price = get_mid_market_rate(tqqq_calls[sqqq_puts.index(sqqq_option)])
    sqqq_put_price = get_mid_market_rate(sqqq_option)
    sqqq_call_price = get_mid_market_rate(sqqq_puts[tqqq_calls.index(tqqq_option)])
    tqqq_call_spread = tqqq_call_price - get_mid_market_rate(tqqq_calls[tqqq_calls.index(tqqq_option) + 1])
    tqqq_put_spread = tqqq_put_price - get_mid_market_rate(tqqq_calls[sqqq_puts.index(sqqq_option) - 1])
    sqqq_put_spread = sqqq_put_price - get_mid_market_rate(sqqq_puts[sqqq_puts.index(sqqq_option) - 1])
    sqqq_call_spread = sqqq_call_price - get_mid_market_rate(sqqq_puts[tqqq_calls.index(tqqq_option) + 1])
    position = {
        "TQQQ Call Spread": {"Buy Price": tqqq_call_price, "Sell Price": None, "Spread": tqqq_call_spread},
        "TQQQ Put Spread": {"Buy Price": None, "Sell Price": None, "Spread": tqqq_put_spread},
        "SQQQ Put Spread": {"Buy Price": sqqq_put_price, "Sell Price": None, "Spread": sqqq_put_spread},
        "SQQQ Call Spread": {"Buy Price": None, "Sell Price": None, "Spread": sqqq_call_spread}
        }
    # Record the position in the portfolio
    portfolio.append(position)
    # Print the position
    print("New Position:")
    print(position)
    #Define a function to close the position
    # def close_position():
    #     global portfolio

    # #Define a function to close the position
    def close_position():
        global portfolio
    # Get the current date
    current_date = datetime.now().date()
    # Check if it is options expiration day or the last working day of the week
    if is_options_expiration_day(current_date) or is_last_working_day_of_week(current_date):

    # Close the position by selling the spreads at the mid-market rate
        position = portfolio[-1]
        tqqq_call_sell_price = get_mid_market_rate(tqqq_calls[tqqq_calls.index(tqqq_option) + 1])
        tqqq_put_sell_price = get_mid_market_rate(tqqq_calls[sqqq_puts.index(sqqq_option) - 1])
        sqqq_put_sell_price = get_mid_market_rate(sqqq_puts[sqqq_puts.index(sqqq_option) - 1])
        sqqq_call_sell_price = get_mid_market_rate(sqqq_puts[tqqq_calls.index(tqqq_option) + 1])
        position["TQQQ Call Spread"]["Sell Price"] = tqqq_call_sell_price
        position["TQQQ Put Spread"]["Sell Price"] = tqqq_put_sell_price
        position["SQQQ Put Spread"]["Sell Price"] = sqqq_put_sell_price
        position["SQQQ Call Spread"]["Sell Price"] = sqqq_call_sell_price
    portfolio[-1] = position

    # Print the position
    print("Closed Position:")
    print(position)

    # Calculate the profit or loss from the position
    pl = calculate_profit_loss(position)
    print(f"Profit/Loss: {pl}")

    # Record the profit or loss in the portfolio
    portfolio[-1]["Profit/Loss"] = pl

    #Define a function to calculate the weekly portfolio return
def calculate_weekly_portfolio_return():
    global portfolio, starting_portfolio_value
    # Get the current date and the last week's date
    current_date = datetime.now().date()
    last_week_date = current_date - timedelta(days=7)
    # Calculate the total value of the portfolio for the current week and the last week
    current_portfolio_value = starting_portfolio_value
    last_week_portfolio_value = starting_portfolio_value
    for position in portfolio:
        if "Sell Price" in position["TQQQ Call Spread"]:
            current_portfolio_value += position["TQQQ Call Spread"]["Sell Price"] - position["TQQQ Call Spread"]["Buy Price"]
            last_week_portfolio_value += position["TQQQ Call Spread"]["Sell Price"] - position["TQQQ Call Spread"]["Buy Price"]
            if "Sell Price" in position["TQQQ Put Spread"]:
                current_portfolio_value += position["TQQQ Put Spread"]["Sell Price"] - position["TQQQ Put Spread"]["Buy Price"]
                last_week_portfolio_value += position["TQQQ Put Spread"]["Sell Price"] - position["TQQQ Put Spread"]["Buy Price"]
                if "Sell Price" in position["SQQQ Put Spread"]:
                    current_portfolio_value += position["SQQQ Put Spread"]["Sell Price"] - position["SQQ"]


    
    
