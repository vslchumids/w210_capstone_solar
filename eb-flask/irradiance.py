import numpy as np
import pandas as pd
import datetime
import calendar
import json
import sys
import date_time_util

from datetime import date, datetime
from calendar import monthrange, isleap

import sys

# Functions for getting irradiance

def convert_str_to_datetime(datetime_str):
    return pd.to_datetime(datetime_str, format='%Y-%m-%d', errors='ignore')

def extract_year(datetime_str):
    return int(datetime_str[:4])

def extract_month(datetime_str):
    return int(datetime_str[5:7])

def get_spi_by_day(station_id, target_year = 2006, file_path = "data/"):    
    file_name = str(station_id) + "_forecasttable.csv"    
    spi = pd.read_csv(file_path + file_name)
    
    spi['ds_date'] = map(convert_str_to_datetime, spi.ds)
    spi = spi[spi['ds_date'] >= pd.Timestamp(str(target_year) + '-01-01 00:00:00')]

    spi_by_day = spi[['ds_date', 'ds', 'yhat']]
    spi_by_day.columns = ['date', 'date_str', 'spi']
    
    return spi_by_day

def get_spi_by_month(station_id, target_year = 2006, file_path = "data/"):    
    spi_by_day = get_spi_by_day(station_id = station_id, 
                                target_year = target_year, 
                                file_path = file_path)
    spi_by_day['month_index'] = map(extract_month, spi_by_day.date_str)
    
    spi_by_month = spi_by_day.groupby(['month_index']).mean()
    spi_by_month['month'] = [spi_by_month.iloc[i].name for i in range(spi_by_month.shape[0])]
        
    return spi_by_month

def get_gen_factor_by_month(station_id, 
                            leap_year = False, 
                            target_year = 2006, 
                            file_path = "data/", 
                            daylight_hours = date_time_util.daylight_hours):
    spi_by_month = get_spi_by_month(station_id)
    gen_factors_by_month = [a * b for a,b in zip(spi_by_month.spi, daylight_hours)]
    
    if leap_year:
        gen_factors_by_month = [a * b for a,b in zip(gen_factors_by_month, date_time_util.num_days_by_month_leap_year)]
    else:
        gen_factors_by_month = [a * b for a,b in zip(gen_factors_by_month, date_time_util.num_days_by_month)]
        
    spi_by_month['gen_factor'] = gen_factors_by_month
    
    return spi_by_month

def main():
    print "Hello from irradince.py"
    print "Number of arguments %d" % len(sys.argv) 
    for i in range(len(sys.argv)):
      print "sys.argv[%d] = %s" % (i, sys.argv[i])  
        
    if(len(sys.argv) < 3):
        file_path = "data/"
    else:        
        file_path = sys.argv[1]
        
    if(len(sys.argv) < 2):
        station_id = 690150
    else:
        station_id = sys.argv[2]  
        
    spi = get_spi_by_month(station_id, 2006, file_path)
    print spi.tail()