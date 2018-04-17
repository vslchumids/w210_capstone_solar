import sys
import workdays

import datetime
import calendar
import numpy as np
import pandas as pd
import json

from datetime import date, datetime, time
from calendar import monthrange, isleap

# Global variables
daylight_hours = [8., 9., 10., 11., 12., 13., 13., 12., 10., 9., 8., 8.]
daylight_hours_pct = [8/24.0, 9/24.0, 10/24.0, 11/24.0, 12/24.0, 13/24.0, 13/24.0, 12/24.0, 10/24.0, 9/24.0, 8/24.0, 8/24.0]
num_days_by_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
num_days_by_month_leap_year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

def get_daylight_hours_by_month():
    return daylight_hours

def get_daylight_hours_pct_by_month():
    return daylight_hours_pct

def get_num_work_hrs_per_week(wd_work_hrs=10,
                              we_work_hrs=12):
    return (wd_work_hrs * 5. + we_work_hrs * 2.)

def get_num_work_hrs_per_day_of_week(wd_work_hrs=10,
                                     we_work_hrs=12):
    return [wd_work_hrs * 1., wd_work_hrs * 1., wd_work_hrs * 1., wd_work_hrs * 1., wd_work_hrs * 1., we_work_hrs * 1., we_work_hrs * 1.]

def get_sun_rise_set_times(target_year = 2018, 
                           city = "San_Francisco", 
                           data_path = "../Data/"):
    # Defaulting to 2018, SF for now
    target_year = 2018 
    city = "San_Francisco"
        
    file_path = data_path + "sun_" + city.lower() + "_" + str(target_year) + ".csv"
    sun_times = pd.read_csv(file_path, header = None)
    sun_times.columns = ["id", 
                         "Jan_Rise",
                         "Jan_Set",
                         "Feb_Rise",
                         "Feb_Set",
                         "Mar_Rise",
                         "Mar_Set",
                         "Apr_Rise",
                         "Apr_Set",
                         "May_Rise",
                         "May_Set",
                         "Jun_Rise", 
                         "Jun_Set",
                         "Jul_Rise",
                         "Jul_Set",
                         "Aug_Rise",
                         "Aug_Set",
                         "Sep_Rise",
                         "Sep_Set",
                         "Oct_Rise",
                         "Oct_Set",
                         "Nov_Rise",
                         "Nov_Set", 
                         "Dec_Rise", 
                         "Dec_Set"]
    
    #sun_times_transformed = sun_times.astype(object).replace(np.nan, 'None')
    sun_times_transformed = sun_times.where((pd.notnull(sun_times)), None)
    
    for i in range(sun_times_transformed.shape[0]):
        for j in range(1, sun_times_transformed.shape[1]):
            if sun_times_transformed.iloc[i, j] != None:
                curr_date = datetime(target_year, int(np.ceil(j / 2.0)), i + 1)
                print curr_date
                hr_str = str(int(sun_times_transformed.iloc[i, j]))[:len(str(int(sun_times_transformed.iloc[i, j]))) - 2]
                min_str = str(int(sun_times_transformed.iloc[i, j]))[-2:]            
                sun_times_transformed.iloc[i, j] = time(int(hr_str), int(min_str))
                #print sun_times_transformed.iloc[i,j]
            else:
                print "sun_times_transformed.iloc[%d, %d] = None" % (i, j)
    return sun_times_transformed