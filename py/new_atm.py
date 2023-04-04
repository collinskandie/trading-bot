import yfinance as yf
from datetime import datetime, timedelta
from py_vollib.black_scholes_merton.implied_volatility import implied_volatility
from py_vollib.black_scholes_merton.greeks.analytical import delta, gamma, theta
from py_vollib.black_scholes_merton.greeks.numerical import rho
import math

# Define the parameters of the strategy
tqqq_ticker = "TQQQ"
sqqq_ticker = "SQQQ"
spread_width = 0.05
start_date = datetime(2022, 1, 1)
end_date = datetime(2022, 12, 31)
portfolio = []


def get_mid_market_rate(option):
    """Calculate the mid-market rate for an option"""
    return (option.bid + option.ask) / 2


def get_expiration_date(options):
    """Get the expiration date of the options"""
    return options[0].expiration


def get_options_chain(ticker):
    """Get the options chain for a given ticker"""
    return yf.Ticker(ticker).options


def get_atm_options(options):
    """Get the at-the-money (ATM) options from a list of options"""
    atm_options = []
    for option in options:
        if option.strike >= options[0].underlying_price * (1 - spread_width / 2) and option.strike <= options[0].underlying_price * (1 + spread_width / 2):
            atm_options.append(option)
    return atm_options


def calculate_greeks(option, price, interest_rate):
    """Calculate the greeks for an option"""
    t = (option.expiration - datetime.today()).days / 365
    iv = implied_volatility(price, option.underlying_price,
                            option.strike, t, interest_rate, 'c')
    d1 = (math.log(option.underlying_price / option.strike) +
          (interest_rate + iv ** 2 / 2) * t) / (iv * math.sqrt(t))
    d2 = d1 - iv * math.sqrt(t)
    delta_val = delta('c', option.underlying_price,
                      option.strike, t, interest_rate, iv)
    gamma_val = gamma('c', option.underlying_price,
                      option.strike, t, interest_rate, iv)
    theta_val = theta('c', option.underlying_price,
                      option.strike, t, interest_rate, iv)
    rho_val = rho('c', option.underlying_price,
                  option.strike, t, interest_rate, iv)
    return delta_val, gamma_val, theta_val, rho_val


def buy_spreads(tqqq_options, sqqq_options, current_date):
    # Buy the call spread and the put spread at the mid-market rate
    tqqq_call_price = get_mid_market_rate(tqqq_option)
    tqqq_put_price = get_mid_market_rate(
        tqqq_calls[sqqq_puts.index(sqqq_option)])
    sqqq_put_price = get_mid_market_rate(sqqq_option)
    sqqq_call_price = get_mid_market_rate(
        sqqq_puts[tqqq_calls.index(tqqq_option)])
    tqqq_call_spread = tqqq_call_price - \
        get_mid_market_rate(tqqq_calls[tqqq_calls.index(tqqq_option) + 1])
    tqqq_put_spread = tqqq_put_price - \
        get_mid_market_rate(tqqq_calls[sqqq_puts.index(sqqq_option) - 1])
    sqqq_put_spread = sqqq_put_price - \
        get_mid_market_rate(sqqq_puts[sqqq_puts.index(sqqq_option) - 1])
    sqqq_call_spread = sqqq_call_price - \
        get_mid_market_rate(sqqq_puts[tqqq_calls.index(tqqq_option) + 1])
    position = {
        "TQQQ Call Spread": {"Buy Price": tqqq_call_price, "Sell Price": None, "Spread": tqqq_call_spread},
        "TQQQ Put Spread": {"Buy Price": tqqq_put_price, "Sell Price": None, "Spread": tqqq_put_spread},
        "SQQQ Call Spread": {"Buy Price": sqqq_call_price, "Sell Price": None, "Spread": sqqq_call_spread},
        "SQQQ Put Spread": {"Buy Price": sqqq_put_price, "Sell Price": None, "Spread": sqqq_put_spread},
    }
    # Loop over the days from start_date to end_date
    for i in range((end_date - start_date).days + 1):
        current_date = start_date + timedelta(days=i)
        # Check if it's a trading day
        if current_date.weekday() < 5:
            # Check if we need to roll the options
            if current_date == get_expiration_date(tqqq_options) or (current_date.weekday() == 4 and get_expiration_date(tqqq_options).weekday() != 4):
                tqqq_options = roll_options(tqqq_options)
                sqqq_options = roll_options(sqqq_options)
                tqqq_atm_options = get_atm_options(tqqq_options)
                sqqq_atm_options = get_atm_options(sqqq_options)
                tqqq_option = tqqq_atm_options[0]
                sqqq_option = sqqq_atm_options[0]
                tqqq_calls = [
                option for option in tqqq_atm_options if option.option_type == 'call']
                sqqq_puts = [
                option for option in sqqq_atm_options if option.option_type == 'put']
                tqqq_call_price = get_mid_market_rate(tqqq_option)
                tqqq_put_price = get_mid_market_rate(
                tqqq_calls[sqqq_puts.index(sqqq_option)])
                sqqq_put_price = get_mid_market_rate(sqqq_option)
                sqqq_call_price = get_mid_market_rate(
                sqqq_puts[tqqq_calls.index(tqqq_option)])
                tqqq_call_spread = tqqq_call_price - \
                get_mid_market_rate(
                    tqqq_calls[tqqq_calls.index(tqqq_option) + 1])
                tqqq_put_spread = tqqq_put_price - get_mid_market_rate
