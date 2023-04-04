#!/usr/bin/env python
# coding: utf-8

# Define Variables and imports

# In[289]:


import yfinance as yf
import datetime as dt
import pandas as pd
import numpy as np
import time

# Set up the start and end dates for the data
start_date = '2020-01-02'
end_date = '2023-04-04'

# Set up the ticker symbols for TQQQ and SQQQ
tqqq_symbol = 'TQQQ'
sqqq_symbol = 'SQQQ'

portfolio_df = pd.DataFrame(columns=['Week End Date','TQQQ Call Sell Price', 'SQQQ Put Sell Price', 'Total Cost', 'Total P/L'])
# Set up the options expiration date as the third Friday of each month
def get_third_friday(year, month):
    d = dt.date(year, month, 15)
    while d.weekday() != 4:
        d += dt.timedelta(1)
    return d

# Set up the strike prices and width of the spreads
spread_width = 0.05
tqqq_strike_price = 0
sqqq_strike_price = 0

# Set up the initial portfolio value
initial_portfolio_value = 10000


# Define a function to calculate the profit/loss of a trade

# In[290]:


def calculate_profit_loss(quantity, buy_price, sell_price):
    return quantity * (sell_price - buy_price)

# Set up an empty list to store the portfolio values over time
portfolio_values = []


# Download the historical data for TQQQ and SQQQ

# In[291]:


tqqq_data = yf.download(tqqq_symbol, start=start_date, end=end_date)
sqqq_data = yf.download(sqqq_symbol, start=start_date, end=end_date)

spread_width = 0.05  # Set the spread width for the call and put spreads
tqqq_data['Strike'] = round(tqqq_data['Close'] / spread_width) * spread_width
sqqq_data['Strike'] = round(sqqq_data['Close'] / spread_width) * spread_width



# In[292]:


#display Data
tqqq_data


# In[293]:


sqqq_data


# Loop over each month in the data and buy the call and put spreads on the third Friday
# 

# In[297]:


# Loop over each month in the data and buy the call and put spreads on the third Friday
for year in range(int(start_date[:4]), int(end_date[:4])+1):
    i= 0
    for month in range(1, 13):
        if year == int(start_date[:4]) and month < int(start_date[5:7]):
            continue
        if year == int(end_date[:4]) and month > int(end_date[5:7]):
            continue

        # Get the expiration date for this month
        expiration_date = get_third_friday(year, month)

        # Only execute the trade on Fridays
        if expiration_date.weekday() != 4:
            continue

        # Get the data for this month
        # tqqq_month_data = tqqq_data.loc[start_date:end_date][str(year)+'-'+str(month)]
        # sqqq_month_data = sqqq_data.loc[start_date:end_date][str(year)+'-'+str(month)]
       # tqqq_month_data = tqqq_data.loc[start_date:end_date][pd.to_datetime(str(year)+'-'+str(month))]
        #sqqq_month_data = sqqq_data.loc[start_date:end_date][pd.to_datetime(str(year)+'-'+str(month))]
        # Get the data for this month
        month_start = pd.to_datetime(str(year)+'-'+str(month)+'-01')
        month_end = month_start + pd.DateOffset(months=1) - pd.Timedelta(days=1)
        tqqq_month_data = tqqq_data.loc[month_start:month_end]
        sqqq_month_data = sqqq_data.loc[month_start:month_end]
        #Test if download is working #commented out code
        # print(tqqq_month_data)
        # print(sqqq_month_data)
        #Find the ATM strike price for TQQQ and SQQQ
        tqqq_close = tqqq_month_data['Close']
        tqqq_strike_price = round(tqqq_close / spread_width) * spread_width
        sqqq_close = sqqq_month_data['Close']
        sqqq_strike_price = round(sqqq_close / spread_width) * spread_width
         # Buy the call and put spreads at the mid-market rate
        tqqq_ask = tqqq_month_data['High'][tqqq_month_data['Strike'] == tqqq_strike_price].values[0]
        tqqq_bid = tqqq_month_data['Low'][tqqq_month_data['Strike'] == tqqq_strike_price].values[0]
        tqqq_mid = (tqqq_ask + tqqq_bid) / 2
        sqqq_ask = sqqq_month_data['High'][sqqq_month_data['Strike'] == sqqq_strike_price].values[0]
        sqqq_bid = sqqq_month_data['Low'][sqqq_month_data['Strike'] == sqqq_strike_price].values[0]
        sqqq_mid = (sqqq_ask + sqqq_bid) / 2
        tqqq_call_buy_price = tqqq_mid
        tqqq_put_buy_price = tqqq_mid
        sqqq_put_buy_price = sqqq_mid 
        tqqq_call_sell_price = tqqq_month_data['Low'][tqqq_month_data['Strike'] == tqqq_strike_price + spread_width].values
        if len(tqqq_call_sell_price) > 0:
            tqqq_call_sell_price = tqqq_call_sell_price[0]
        else:
            # handle the case where the filter did not match any rows
            tqqq_call_sell_price = 0.0  # set some default value        
        tqqq_put_sell_price = sqqq_month_data['Low'][sqqq_month_data['Strike'] == sqqq_strike_price - spread_width].values
        if len(tqqq_put_sell_price) > 0:
            tqqq_put_sell_price = tqqq_put_sell_price[0]
        else:
            # handle the case where the filter did not match any rows
            tqqq_put_sell_price = 0.0  # set some default value 
           
        sqqq_call_sell_price = sqqq_month_data['Low'][sqqq_month_data['Strike'] == sqqq_strike_price + spread_width].values
        if len(sqqq_call_sell_price) > 0:
            sqqq_call_sell_price = sqqq_call_sell_price[0]
        else:
            # handle the case where the filter did not match any rows
            sqqq_call_sell_price = 0.0  # set some default value 

        sqqq_put_sell_price = sqqq_month_data['Low'][sqqq_month_data['Strike'] == sqqq_strike_price - spread_width].values
        if len(sqqq_put_sell_price) > 0:
            sqqq_put_sell_price = sqqq_put_sell_price[0]
        else:
            # handle the case where the filter did not match any rows
            sqqq_put_sell_price = 0.0  # set some default value 
        #tqqq_put_sell_price = sqqq_month_data['Low'][sqqq_month_data['Strike'] == sqqq_strike_price - spread_width].values[0] 
        # Record the trade in the portfolio
        tqqq_call_quantity = int(initial_portfolio_value / tqqq_call_buy_price)
        sqqq_put_quantity = int(initial_portfolio_value / sqqq_put_buy_price)
        tqqq_call_profit_loss = calculate_profit_loss(tqqq_call_quantity, tqqq_call_buy_price, tqqq_call_sell_price)
        tqqq_put_quantity = int(initial_portfolio_value / tqqq_put_buy_price)
        tqqq_put_profit_loss = calculate_profit_loss(tqqq_put_quantity, tqqq_put_buy_price, tqqq_put_sell_price)
        total_cost = (tqqq_call_buy_price + sqqq_put_buy_price)
        # Calculate the total profit/loss for this trade
        total_profit_loss = tqqq_call_profit_loss + tqqq_put_profit_loss
        # Update the portfolio value
        initial_portfolio_value += total_profit_loss
        portfolio_values.append(initial_portfolio_value)    
        # Iterate over weeks
        # # Append row to portfolio dataframe        
        portfolio_df = pd.concat([portfolio_df, pd.DataFrame({'Week End Date': expiration_date,                                         
                                        'TQQQ Call Sell Price': tqqq_call_sell_price,                                        
                                        'SQQQ Put Sell Price': sqqq_put_sell_price, 
                                        'Total Cost': total_cost, 
                                        'Total P/L': total_profit_loss}, index=[i+1])])
        i +=1
        # Print the trade details
        # print(f"Trade Date: {expiration_date}\n"
        #       f"TQQQ Call Buy Price: {tqqq_call_buy_price:.2f}\n"
        #       f"TQQQ Call Sell Price: {tqqq_call_sell_price:.2f}\n"
        #       f"TQQQ Call Quantity: {tqqq_call_quantity}\n"
        #       f"TQQQ Call P/L: {tqqq_call_profit_loss:.2f}\n"
        #       f"SQQQ Put Buy Price: {sqqq_put_buy_price:.2f}\n"
        #       f"SQQQ Put Sell Price: {sqqq_put_sell_price:.2f}\n"
        #       f"SQQQ Put Quantity: {sqqq_put_quantity}\n"
        #       f"Total P/L: {total_profit_loss:.2f}\n"
        #       f"Portfolio Value: {initial_portfolio_value:.2f}\n"
        #       f"-----------------------------------------")

        # Wait for a few seconds before executing the next trade
        #time.sleep(5)


# Print the final portfolio value

# In[296]:


print(f"Final Portfolio Value: {initial_portfolio_value:.2f}")


# Plot the portfolio values over time

# In[295]:


# Plot the portfolio values over time
portfolio_data = pd.DataFrame(portfolio_values, columns=['Value'])
portfolio_data.plot(figsize=(10,5), title='Portfolio Value Over Time')
# print portfolio
portfolio_df 

