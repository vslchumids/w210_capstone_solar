import pandas as pd
import numpy as np
import sys
import workdays
import datetime

from calendar import monthrange, isleap

def get_monthly_num_workhours_pct(year=None, 
                                  leap_year=0, 
                                  operating_hours=[8., 8., 8., 8., 8., 0., 0.]):
    """Returns number of days and corresponding % for each month in a year 

    Args:
        year (int): The target year to be considered
        leap_year (int): Whether a leap_year is considered or not: Overridden if year is valid
        operating_hours (list of floats): Number of operatings for each day of week, starting with Monday;
                                          Only used if a valid year is passed as parameter

    Returns:
        Data frame with number of days and corresponding % for each month in a year 
    """    
    
    monthly_df = pd.DataFrame()
    monthly_df['month'] = [i + 1 for i in range(12)]
    
    daily_df = pd.DataFrame()
    
    if year == None or year < 2000:
        is_leap_year = leap_year
        if is_leap_year == 0:
            monthly_df['num_days'] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            days_count = 365
        else:
            monthly_df['num_days'] = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            days_count = 366
            
        num_workdays_per_wk = len([i for i in range(len(operating_hours)) if operating_hours[i] > 0])
        monthly_df['num_days'] = monthly_df['num_days'] * (num_workdays_per_wk / 7.0)
        monthly_df['num_hours'] = monthly_df['num_days'] * 8
        
        total_num_days = sum(monthly_df['num_days'])
        monthly_df['num_days_pct'] = monthly_df['num_days'] / total_num_days
        monthly_df['num_hours_pct'] = monthly_df['num_days_pct']
        
        # Set values for daily_hrs
        daily_hrs_index = 0        
        daily_df['num_hours'] = np.zeros(days_count)
        daily_df['num_hours_pct'] = np.zeros(days_count)
        daily_df['date'] = None
    else:   
        monthly_df['num_days'] = np.zeros(12)        
        monthly_df['num_hours'] = np.zeros(12)
        
        is_leap_year = isleap(year)        
        if is_leap_year == 0:
            days_count = 365
        else:
            days_count = 366   
            
        daily_hrs_index = 0
        daily_df['num_hours'] = np.zeros(days_count)
        daily_df['num_hours_pct'] = np.zeros(days_count)
        daily_df['date'] = None
        
        for i in range(12):
            temp_num_days = monthrange(year, i + 1)[1]
            temp_num_weekdays = np.zeros(7)
            
            for j in range(temp_num_days):
                day_of_week_index = datetime.datetime(year, i + 1, j + 1).weekday()
                temp_num_weekdays[day_of_week_index] = temp_num_weekdays[day_of_week_index] + 1
                
                #daily_hrs[daily_hrs_index] = operating_hours[day_of_week_index]
                daily_df.loc[daily_hrs_index, 'num_hours'] = operating_hours[day_of_week_index]
                daily_df.loc[daily_hrs_index, 'date'] = datetime.datetime(year, i + 1, j + 1)
                daily_hrs_index = daily_hrs_index + 1
        
            temp_num_work_hours = [x * y for x, y in zip(temp_num_weekdays, operating_hours)]
            monthly_df.loc[monthly_df['month'] == i + 1, 'num_hours'] = sum(temp_num_work_hours)
            
            temp_num_work_days = [x * y for x, y in zip(temp_num_weekdays, [1.0 if operating_hours[k] > 0 else 0.0 for k in range(len(operating_hours))])]
            monthly_df.loc[monthly_df['month'] == i + 1, 'num_days'] = sum(temp_num_work_days) 

        total_num_days = sum(monthly_df['num_days'])
        monthly_df['num_days_pct'] = monthly_df['num_days'] / total_num_days            
        total_num_hours = sum(monthly_df['num_hours'])
        monthly_df['num_hours_pct'] = monthly_df['num_hours'] / total_num_hours   
        
        total_num_hours = sum(daily_df['num_hours'])
        daily_df['num_hours_pct'] = daily_df['num_hours'] / total_num_hours   
        
    return monthly_df, daily_df

def get_monthly_num_days_pct(year=None, leap_year=0):
    """Returns number of days and corresponding % for each month in a year 

    Args:
        year (int): The target year to be considered
        leap_year (int): Whether a leap_year is considered or not: Overridden if year is valid

    Returns:
        Data frame with number of days and corresponding % for each month in a year 
    """    
    monthly_df = pd.DataFrame()
    monthly_df['month'] = [i + 1 for i in range(12)]
    if year == None:
        is_leap_year = leap_year
    else:    
        is_leap_year = isleap(year)
        
    if is_leap_year == 0:
        monthly_df['num_days'] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    else:
        monthly_df['num_days'] = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    
    total_num_days = sum(monthly_df['num_days'])
    monthly_df['num_days_pct'] = monthly_df['num_days'] / total_num_days
    return monthly_df

def get_annual_consumption(ols_usable, 
                           biz_type, 
                           wkrs, 
                           sqft, 
                           wkhrs=40,
                           heat=0, 
                           cook=0, 
                           cool=0, 
                           manu=0, 
                           water=0, 
                           elvr=0, 
                           escl=0, 
                           opn24=0, 
                           rfrd=0,
                           opnwe=0):
    """Return annual consumption based on the business type, number of workers, square footage of the 
       commercial space, etc. entered by the user

    Args:
        ols_usable (dataframe): Data frame with coefficients of the consumption regression model;
                                Expected to be passed in after getting the data from the data lake  
        biz_type (string): Type of business
        wkrs (int): Number of workers
        sqft (float): Area of commercial space in square footage 
        wkhrs (int): Number of work hours per week
        heat (int): Whether electricity is used for heating
        cook (int): Whether electricity is used for cooking
        cool (int): Whether electricity is used for cooling
        manu (int): Whether electricity used for manufacturing
        water (int): Whether electricity is used for water heating
        elvr (int): Number of elevators
        escl (int): Number of escalators
        opn24 (int) Whether or not the business opens 24 hours
        rfrd (int): Whether there is refrigeration
        opnwe (int): Whether or not the busiess opens during the weekend

    Returns:
        Annual consumption of the small / mid size business
    """
    
    return (ols_usable.loc[ols_usable['business_type'] == biz_type]['ELCOOK'] * cook +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['ELCOOL'] * cool +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['ELHT1'] * heat +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['ELMANU'] * manu +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['ELWATR'] * water +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['NELVTR'] * elvr +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['NESLTR'] * escl +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['NWKER'] * wkrs +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['OPEN24'] * opn24 +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['RFGEQP'] * rfrd +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['SQFT'] * sqft +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['WKHRS'] * wkhrs +
            ols_usable.loc[ols_usable['business_type'] == biz_type]['OPNWE'] * opnwe).iloc[0]

def get_monthly_daily_consumption(ols_usable, 
                                  biz_type, 
                                  wkrs, 
                                  sqft, 
                                  wkhrs=40,
                                  heat=0, 
                                  cook=0, 
                                  cool=0, 
                                  manu=0, 
                                  water=0, 
                                  elvr=0, 
                                  escl=0, 
                                  opn24=0, 
                                  rfrd=0, 
                                  opnwe=0, 
                                  year=None, 
                                  leap_year=0, 
                                  operating_hours=[8., 8., 8., 8., 8., 0., 0.]):

    """Return monthly and daily consumption based on the business type, number of workers, 
       square footage of the commercial space, year, operating hours, etc. entered by the user

    Args:
        ols_usable (dataframe): Data frame with coefficients of the consumption regression model;
                                Expected to be passed in after getting the data from the data lake  
        biz_type (string): Type of business
        wkrs (int): Number of workers
        sqft (float): Area of commercial space in square footage 
        wkhrs (int): Number of work hours per week
        heat (int): Whether electricity is used for heating
        cook (int): Whether electricity is used for cooking
        cool (int): Whether electricity is used for cooling
        manu (int): Whether electricity used for manufacturing
        water (int): Whether electricity is used for water heating
        elvr (int): Number of elevators
        escl (int): Number of escalators
        opn24 (int) Whether or not the business opens 24 hours
        rfrd (int): Whether there is refrigeration
        opnwe (int): Whether or not the busiess opens during the weekend
        year (int): The target year to be considered
        leap_year (int): Whether a leap_year is considered or not: Overridden if year is valid
        operating_hours (list of floats): Number of operatings for each day of week, starting with Monday;
                                          Only used if a valid year is passed as parameter

    Returns:
        Annual consumption of the small / mid size business
    """
    
    annual_cons = get_annual_consumption(ols_usable, 
                                         biz_type, 
                                         wkrs, 
                                         sqft, 
                                         wkhrs,
                                         heat, 
                                         cook, 
                                         cool, 
                                         manu, 
                                         water, 
                                         elvr, 
                                         escl, 
                                         opn24, 
                                         rfrd,
                                         opnwe)

    #monthly_cons = get_monthly_num_days_pct(year, leap_year)
    monthly_cons, daily_cons = get_monthly_num_workhours_pct(year=year, 
                                                             leap_year=leap_year, 
                                                             operating_hours=operating_hours)    
    monthly_cons['monthly_consumption'] = monthly_cons['num_hours_pct'] * annual_cons
    daily_cons['daily_consumption'] = daily_cons['num_hours_pct'] * annual_cons
    
    return monthly_cons, daily_cons
    
def main():
    print "Hello from consumption.py"
    print "Number of arguments %d" % len(sys.argv) 
    for i in range(len(sys.argv)):
      print "sys.argv[%d] = %s" % (i, sys.argv[i])
        
    if(len(sys.argv) < 6):
        target_year = 2018
    else:
        target_year = sys.argv[5]
        
    if(len(sys.argv) < 5):
        sqft = 1000
    else:
        sqft = sys.argv[4]

    if(len(sys.argv) < 4):
        num_workers = 10
    else:
        num_workers = sys.argv[3]       
        
    if(len(sys.argv) < 3):
        business_type = "Office"
    else:
        business_type = sys.argv[2]    
        
    if(len(sys.argv) < 2):
        file_path = "../Data/ols_usable.csv"
    else:        
        file_path = sys.argv[1]
        
    ols_usable = pd.read_csv(file_path)
    monthly_cons, daily_cons = get_monthly_daily_consumption(ols_usable, business_type, num_workers, sqft, year=target_year)
    print "Monthly Consumption:", monthly_cons
    print "Daily Consumption:", daily_cons
        
if __name__== "__main__":
  main()
