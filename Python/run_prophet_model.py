import pandas as pd
import numpy as np
import math
from argparse import ArgumentParser
from fbprophet import Prophet

# To preprocess the data frame and caculate the GHI
def preprocess(df):
    # Select columns
    df = df.loc[:, ['YYYY-MM-DD', 'HH:MM (LST)', 'Zenith (deg)', 'Dir Mod (W/m^2)', \
                    'Dif Mod  (W/m^2)', 'Meas Dir (W/m^2)', 'Meas Dif (W/m^2)',
                    'Precip Wat (cm)', 'AOD (unitless)']]
    
    # Convert degree to radian
    df['Zenith (deg)'] = df['Zenith (deg)'].apply(math.radians)
    df = df.rename({'Zenith (deg)': 'Zenith (rad)'}, axis = 1)
    
    # Calculate GHI
    df['GHI'] = df['Dif Mod  (W/m^2)'].astype('float') + \
                df['Dir Mod (W/m^2)']. astype('float') * df['Zenith (rad)'].apply(math.cos)

    return df

def main(station_table, station_id):
    
    future_series = station_id + '_future.csv'
    forecast_table = station_id + '_forecasttable.csv'
    
    # Preprocess data
    print "Preprocess Data ... "
    dataset_post = preprocess(station_table)
    dataset_post['YYYY-MM-DD'] = pd.to_datetime(dataset_post['YYYY-MM-DD'])  
    dataset_post = dataset_post.rename({'YYYY-MM-DD':'Datetime'}, axis = 1)
    
    # Create daily series in the format works for prophet
    print "Create Series for Modeling ... "
    daily = dataset_post.groupby('Datetime')
    daily_data = daily['Datetime', 'GHI'].mean()
    daily_data['SPI'] = daily_data['GHI']/max(daily_data['GHI'])
    daily_ts = daily_data.astype('d')
    ds = daily_ts.loc[:, ['SPI']]
    ds.rename({'SPI':'y'}, axis = 1, inplace = True)
    ds['ds'] = ds.index
    ds = ds.reset_index()
    ds.drop(columns=['Datetime'], inplace=True)
    
    # Run prophet model to forecast a year
    print "Use Prophet to forecast ..."
    m = Prophet()
    m.fit(ds)
    future = m.make_future_dataframe(periods = 365)
    forecast = m.predict(future)
    
    # Output table
    print "Output forecast tables ..."
    forecast.to_csv(forecast_table)

## How to run
## python get_data.py -i weather_station.csv
if __name__ == '__main__':

    # Define the argument parser
    parser = ArgumentParser(description="Run and gridsearch SARIMA time series model on weather station data, then\
                                         select and save the best model based on defined metrics.")
    parser.add_argument("-i", "--input",
                        dest="station_table",
                        default='weather_station.csv',
                        help="Weather Station Data Table in CSV")
    args = parser.parse_args()
    station_table = pd.read_csv(args.station_table)
    station_id = args.station_table.split('.')[0]
    print type(station_table)
    main(station_table, station_id) 
