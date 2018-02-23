import pandas as pd
import numpy as np
import itertools
import sys
import re
import os
import subprocess
import math
from argparse import ArgumentParser

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

def yearly_aggregation(table, station):
    dataset_post = preprocess(table)
    dataset_post['YYYY-MM-DD'] = pd.to_datetime(dataset_post['YYYY-MM-DD'])
    dataset_post = dataset_post.rename({'YYYY-MM-DD':'Datetime'}, axis = 1)
    daily = dataset_post.groupby('Datetime')
    daily_data = daily['Datetime', 'GHI', 'Precip Wat (cm)', 'AOD (unitless)'].mean()
    yearly = daily_data.resample('Y').mean()
    yearly['USAF'] = int(station)
    return yearly

def main(input_table, output_table):

    # Load California station meta table and clean up
    stations_CA = pd.read_csv('NSRDB_CA_StationsMeta.csv')
    station_CA = stations_CA.loc[:, ['USAF','CLASS','STATION', 'lat', 'long']]

    # Reading station CSV file
    data_dir = 'nsrdb_solar'
    dfs = []
    for station in os.listdir(data_dir):
        file_dest = os.path.join(data_dir, station, station + '_2005.csv')
        print "Processing {:s}".format(file_dest)
        table = pd.read_csv(file_dest)
        dfs.append(yearly_aggregation(table, station))

    # Create yearly average table
    weather_stations_agg = pd.concat(dfs)
    weather_stations_agg = weather_stations_agg.reset_index()
    weather_stations_agg = weather_stations_agg.loc[:, ['GHI', 'Precip Wat (cm)', 'AOD (unitless)', 'USAF']]

    # Merge with station table
    feature_table = station_CA.merge(weather_stations_agg, on = 'USAF')
    feature_table.to_csv(output_table, encoding = 'utf-8', index=False)

## How to run
## python get_data.py -c NSRDB_CA_StationsMeta.csv -o output
if __name__ == '__main__':

    # Define the argument parser
    parser = ArgumentParser(description="Download CA Station data based on station table")
    parser.add_argument("-i", "--input",
                        dest="station_table",
                        default='NSRDB_CA_StationsMeta.csv',
                        help="California Station Desc Table")

    parser.add_argument("-o", "--output",
                        dest="output_file",
                        default='feature_table.csv',
                        help="Output Features Table")
    args = parser.parse_args()
    main(args.station_table, args.output_file)
