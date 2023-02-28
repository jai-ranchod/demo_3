# Imports
import numpy as np
from pandas_datareader import data as pdr
from yahoo_fin import stock_info as si
from pandas import ExcelWriter
import yfinance as yf
import pandas as pd
import datetime
yf.pdr_override()

# parameters
filepath = '/Users/jairanchod/Documents/PythonProjects/StockTickerData.xlsx'
lookback_days = 365
top_pct = 0.3
index_name = '^GSPC'  # S&P 500 ticker ID


# Variables
tickers = si.tickers_sp500()
tickers = np.array(tickers)
tickers = [item.replace(".", "-") for item in tickers]  # Yahoo Finance uses dashes instead of dots
start_date = datetime.datetime.now() - datetime.timedelta(
    days=lookback_days)  # defining the start date to be 365 days in the past
end_date = datetime.date.today()  # defining end date to be today

returns_multiples = []

index_df = pdr.get_data_yahoo(index_name, start_date,
                              end_date)  # using pdr to get start and end dates from yahoo finance for S&P 500
# recall we set index name above
index_df['Percent Change'] = index_df[
    'Adj Close'].pct_change()  # using adjusted close to determine % change day over day
# the pct_change() method determines % change based on n-1 vs. n elements
index_return = (index_df['Percent Change'] + 1).cumprod()[
    -1]  # cumulative index return over 1 year; <this number> - 1 is the % yearly return

# Find top 30% performing stocks (relative to the S&P 500)
i = 0

for ticker in tickers:  # tickers defined above; pulls all ticker IDs from S & P 500
    # Download historical data as CSV for each stock (makes the process faster)
    df = pdr.get_data_yahoo(ticker, start_date, end_date)
    # df.to_csv(f'{ticker}.csv')

    # Calculating returns relative to the market (returns multiple)
    df['Percent Change'] = df['Adj Close'].pct_change()
    stock_return = (df['Percent Change'] + 1).cumprod()[-1]  # same return calculation process as above

    returns_multiple = round((stock_return / index_return), 2)
    returns_multiples.extend([returns_multiple])
    i = i + 1
    print(i / len(tickers))
    # print(f'Ticker: {ticker}; Returns Multiple against S&P 500: {returns_multiple}\n')
    # time.sleep(1)

# Creating dataframe of only top 30% of ticker UDs based on returns multiple
# creating initial dataframe with "zip()" function
rs_df = pd.DataFrame(list(zip(tickers, returns_multiples)), columns=['Ticker', 'Returns_multiple'])
# Instantiating rank column as percentage
rs_df['RS_Rating'] = rs_df.Returns_multiple.rank(pct=True)
# Limiting the table to only those entries in the top 30% of the tickers considered
rs_df = rs_df[rs_df.RS_Rating >= rs_df.RS_Rating.quantile(1 - top_pct)]
# Ordering by RS_Rating
rs_df.sort_values(by=['RS_Rating'])

# rs_df = pd.merge(rs_df, exportList, left_on='Ticker', right_on='Stock', how='inner')

writer = pd.ExcelWriter(filepath, engine='xlsxwriter')
rs_df.to_excel(writer, sheet_name='Sheet1', startrow=1, index=False)
# Get the xlsxwriter workbook and worksheet objects.
workbook = writer.book
worksheet = writer.sheets['Sheet1']

# Get the dimensions of the dataframe.
(max_row, max_col) = rs_df.shape

# Create a list of column headers, to use in add_table().
column_settings = [{'header': column} for column in rs_df.columns]

# Add the Excel table structure. Pandas will add the data.
worksheet.add_table(0, 0, max_row, max_col - 1, {'columns': column_settings})

# Make the columns wider for clarity.
worksheet.set_column(0, max_col - 1, 12)

# Close the Pandas Excel writer and output the Excel file.
writer.close()
print('Process Finished')
