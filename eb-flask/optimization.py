import datetime
import calendar
import numpy as np
import pandas as pd
import json
import sys

from pyomo.environ import *
from pyomo.opt import SolverFactory
from datetime import date, datetime, time
from calendar import monthrange, isleap

import date_time_util

from consumption import get_monthly_num_workhours_pct, get_monthly_num_days_pct, get_annual_consumption, get_monthly_daily_consumption
from irradiance import get_spi_by_day, get_spi_by_month, get_gen_factor_by_month
from date_time_util import get_sun_rise_set_times, get_daylight_hours_by_month, get_daylight_hours_pct_by_month, get_num_work_hrs_per_week, get_num_work_hrs_per_day_of_week
from incentive import get_incentive_at_station

#########################################################################################
# Globals
#########################################################################################

# Max KW per square feet of roof top space
kw_per_sqft = 0.015
start_year = 2018 
num_years = 10 
file_path = "data/ols_usable.csv" # path in AWS EB EC2 (eb-flask)

panels_per_kw = 5
annual_clean_cost_per_panel = 15.
annual_inspection_cost = 150.

#########################################################################################
# Functions
#########################################################################################

#========================================================================================
# Compile data into list or dictionary to initialize model params
#========================================================================================

def get_month_ind_list(num_years):
    month_ind_list = []
    k = 1
    for i in range(num_years):    
        for j in range(12):
            month_ind_list.append(k)
            k = k + 1
            
    return month_ind_list
    
def get_cons_dict(business_type, 
                  num_workers, 
                  sqft, 
                  start_year,
                  num_years,
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
                  operating_hours=[8., 8., 8., 8., 8., 0., 0.],
                  file_path = file_path,
                  granularity = 'monthly', 
                  daylight_hours_pct = date_time_util.daylight_hours_pct):  
    
    ols_usable = pd.read_csv(file_path)
    
    cons_day_dict = {}
    cons_night_dict = {}
    
    night_hours_pct = [1 - x for x in daylight_hours_pct]
    
    if(granularity == 'monthly'):
        k = 1
        for i in range(num_years):
            monthly_cons, daily_cons = get_monthly_daily_consumption(ols_usable, 
                                                                     business_type, 
                                                                     num_workers, 
                                                                     sqft, 
                                                                     wkhrs=wkhrs,
                                                                     heat=heat, 
                                                                     cook=cook, 
                                                                     cool=cool, 
                                                                     manu=manu, 
                                                                     water=water, 
                                                                     elvr=elvr, 
                                                                     escl=escl, 
                                                                     opn24=opn24, 
                                                                     rfrd=rfrd, 
                                                                     opnwe=opnwe, 
                                                                     operating_hours=operating_hours, 
                                                                     year = start_year + i)
            monthly_cons_day = [a * b for a,b in zip(monthly_cons.monthly_consumption, daylight_hours_pct)]
            monthly_cons_night = [a * b for a,b in zip(monthly_cons.monthly_consumption, night_hours_pct)]
    
            for j in range(12): 
                cons_day_dict[k] = monthly_cons_day[j]
                cons_night_dict[k] = monthly_cons_night[j]
                k = k + 1
                
    return cons_day_dict, cons_night_dict
        
# Generate .tab file for model.gen_factor
def get_gen_factor_dict(start_year,
                        num_years, 
                        station_id = 690150,
                        granularity = 'monthly'):
    gen_factor_dict = {}
    
    gen_fac = get_gen_factor_by_month(station_id, leap_year = False)
    gen_fac_leap_year = get_gen_factor_by_month(station_id, leap_year = True)
    
    if(granularity == 'monthly'):   
        k = 1
        for i in range(num_years):
            for j in range(12):
                if isleap(start_year + i):
                    gen_factor_dict[k] = gen_fac_leap_year.gen_factor[j + 1]
                else:
                    gen_factor_dict[k] = gen_fac.gen_factor[j + 1]
                k = k + 1
        
    return gen_factor_dict    
    
def get_gen_cost_dict(num_years, 
                      initial_cost, 
                      raise_pct):
    gen_cost_dict = {}
    
    factor = 1 + raise_pct
    k = 1
    for i in range(num_years):    
        for j in range(12):
            gen_cost_dict[k] = initial_cost * (factor ** i)
            k = k + 1
            
    return gen_cost_dict

def get_annual_inspection_cost_dict(num_years, 
                                    ann_insp_cost = annual_inspection_cost):
    
    inspection_cost_dict = {}
    k = 1
    for i in range(num_years):    
        for j in range(12):
            if j == 0:
                inspection_cost_dict[k] = ann_insp_cost
            else:
                inspection_cost_dict[k] = 0
            k = k + 1
            
    return inspection_cost_dict

def get_annual_cleaning_cost_by_kw_dict(num_years, 
                                        clean_cost_panel = annual_clean_cost_per_panel, 
                                        num_panels_per_kw = panels_per_kw):
    
    cleaning_cost_dict = {}
    k = 1
    for i in range(num_years):    
        for j in range(12):
            if j == 0 and i > 0:
                cleaning_cost_dict[k] = clean_cost_panel * num_panels_per_kw
            else:
                cleaning_cost_dict[k] = 0
            k = k + 1
            
    return cleaning_cost_dict

#========================================================================================
# Define Rules for Objective
#========================================================================================
          
def day_nem_tou(model):
    return sum(model.gen_factor[i] * model.x[i] - model.cons_day[i] for i in model.month_ind) * model.nem
    
def night_tou(model):
    return sum(model.cons_night[i] * model.tou_dict[i] for i in model.month_ind)

def gen_costs(model):
    return sum(model.gen_factor[i] * model.x[i] * model.gen_cost[i] for i in model.month_ind)

def system_costs(model):
    return sum((model.x[i] - model.x[i - 1]) * model.system_cost for i in range(2, 121)) + model.x[1] * model.system_cost
    
def inpection_costs(model):
    return sum(model.inpect_cost[i] for i in model.month_ind)

def cleaning_costs(model):
    return sum(model.clean_cost_per_kw[i] * model.x[i] for i in model.month_ind)

def roi_rule(model):
    return day_nem_tou(model) - system_costs(model) - gen_costs(model) - inpection_costs(model) - cleaning_costs(model)

#========================================================================================
# Define Rules for Constraints
#========================================================================================
        
def max_sys_capacity_rule(model, i):
    return model.x[i] <= (model.roof_sqft[i] * kw_per_sqft)

def inc_sys_capacity_rule(model, i):
    if i <= 1:
        return model.x[i] >= -1000000
    else:
        return model.x[i] >= model.x[i - 1]

#========================================================================================
# Define Rules for Model Outputs
#========================================================================================

def gen_kwh_by_month_ind(model):
    return [model.gen_factor[i] * model.x[i] for i in model.month_ind]

def gen_kwh(model):
    return sum(model.gen_factor[i] * model.x[i] for i in model.month_ind)

#========================================================================================
# Create and run model
#========================================================================================

def create_model_sets(model):
    # Month index
    month_ind_list = get_month_ind_list(10)
    model.month_ind = Set(initialize = month_ind_list)

def create_model_vars(model):
    # Recommended system capacity in KW
    model.x = Var(model.month_ind, within=NonNegativeReals, bounds = (1.0, 5000.0), initialize = 1.)    

def create_model_parameters(model, 
                            station_id, 
                            business_type, 
                            roof_sqft,
                            num_workers, 
                            sqft,
                            start_year,
                            num_years,
                            wd_start_hr = 8, 
                            wd_work_hrs = 10,
                            we_start_hr = 10, 
                            we_work_hrs = 12,                            
                            opn24 = 0,
                            heat = 0, 
                            cool = 0,
                            opnwe = 0,
                            cook = 0, 
                            rfrd = 0,
                            manu = 0, 
                            water = 0, # not included in UI, default to 0 
                            elvr = 0, # not included in UI, default to 0
                            escl = 0, # not included in UI, default to 0
                            file_path = file_path):
    # roof size
    model.roof_sqft = Param(model.month_ind, default=roof_sqft)

    # Variable system cost per KW -- initial cost for setting up the solar system
    model.system_cost = Param(default=3750.) 

    # Sample financing parameters: (num_years, 0.162, 0.01)
    gen_cost_dict = get_gen_cost_dict(num_years, 0., 0.)
    model.gen_cost = Param(model.month_ind, within=NonNegativeReals, initialize=gen_cost_dict) # per KWh

    # Variable consumption cost per KWh
    model.tou = Param(default=0.29361) 
    # model.tou_on_peak = Param(default=0.33661) 
    # model.tou_off_peak = Param(default=0.25061) 
    model.tou_dict = get_gen_cost_dict(num_years, 0.29361, 0.01)

    # Variable generation credit per KWh
    model.nem = Param(default=0.29361) # per KWh
    model.nem_on_peak = Param(default=0.33661) # per KWh 
    model.nem_off_peak = Param(default=0.25061) # per KWh 

    # Maintenance costs
    model.inpect_cost = get_annual_inspection_cost_dict(num_years = num_years)
    model.clean_cost_per_kw = get_annual_cleaning_cost_by_kw_dict(num_years = num_years)
    
    # Incentive
    incent, cap = get_incentive_at_station(station_id = station_id)
    if incent != None and cap != None:
        model.incentive_per_kw = incent
        model.incentive_cap = cap
    else:
        model.incentive_per_kw = 0.
        model.incentive_cap = 0.    
        
    print "From create_model_parameters: incent = %f" % incent
    print "From create_model_parameters: cap = %f" % cap
    
    # Convert start hour & work hours by weekday vs. weekend into
    # total number of work hours 
    wkhrs_week = get_num_work_hrs_per_week(wd_work_hrs, we_work_hrs)
    
    # Convert start hour & work hours by weekday vs. weekend into
    # number of work hours per day of week
    op_hours = get_num_work_hrs_per_day_of_week(wd_work_hrs, we_work_hrs)
    
    # Daytime and nighttime consumption by month
    cons_day_dict, cons_night_dict = get_cons_dict(business_type, 
                                                   num_workers, 
                                                   sqft, 
                                                   start_year,
                                                   num_years,
                                                   wkhrs = wkhrs_week,
                                                   heat = heat, 
                                                   cook = cook, 
                                                   cool = cool, 
                                                   manu = manu, 
                                                   water = water, 
                                                   elvr = elvr, 
                                                   escl = escl, 
                                                   opn24 = opn24, 
                                                   rfrd = rfrd, 
                                                   opnwe = opnwe,  
                                                   operating_hours = op_hours,
                                                   file_path = file_path,
                                                   granularity = 'monthly', 
                                                   daylight_hours_pct = date_time_util.daylight_hours_pct)
    model.cons_day = Param(model.month_ind, within=NonNegativeReals, initialize=cons_day_dict)
    model.cons_night = Param(model.month_ind, within=NonNegativeReals, initialize=cons_night_dict)

    # Generation factors = average SPI by day * number of daylight hours
    gen_factor_dict = get_gen_factor_dict(start_year,
                                          num_years, 
                                          station_id = station_id,
                                          granularity = 'monthly')
    model.gen_factor = Param(model.month_ind, within=NonNegativeReals, initialize=gen_factor_dict)

def create_model_constraints(model):
    model.max_syscap_constrait = Constraint(model.month_ind, rule=max_sys_capacity_rule)
    model.inc_syscap_constrait = Constraint(model.month_ind, rule=inc_sys_capacity_rule)

def create_model_objective(model):
    model.savings = Objective(rule = roi_rule, sense = maximize) 

def create_model(station_id, 
                 business_type, 
                 roof_sqft,
                 num_workers, 
                 sqft,
                 start_year,
                 num_years,
                 wd_start_hr = 8, 
                 wd_work_hrs = 10,
                 we_start_hr = 10, 
                 we_work_hrs = 12,                            
                 opn24 = 0,
                 heat = 0, 
                 cool = 0,
                 opnwe = 0,
                 cook = 0, 
                 rfrd = 0,
                 manu = 0, 
                 water = 0, # not included in UI 
                 elvr = 0, # not included in UI
                 escl = 0, # not included in UI
                 file_path = file_path):    
    # Create an abstract model
    model = AbstractModel()

    create_model_sets(model)
    
    create_model_vars(model)
    
    create_model_parameters(model, 
                            station_id = station_id, 
                            business_type = business_type, 
                            roof_sqft = roof_sqft,
                            num_workers = num_workers, 
                            sqft = sqft,
                            start_year = start_year,
                            num_years = num_years,
                            wd_start_hr = wd_start_hr, 
                            wd_work_hrs = wd_work_hrs,
                            we_start_hr = we_start_hr, 
                            we_work_hrs = we_work_hrs,                            
                            opn24 = opn24,
                            heat = heat, 
                            cool = cool,
                            opnwe = opnwe,
                            cook = cook, 
                            rfrd = rfrd,
                            manu = manu, 
                            water = water, # not included in UI 
                            elvr = elvr, # not included in UI
                            escl = escl, # not included in UI
                            file_path = file_path)
        
    create_model_constraints(model)
    
    create_model_objective(model)

    return model

def run_model(model):
    # Create a solver based on Gnu Linear Programming Kit (glpk)
    opt = SolverFactory("glpk")
    
    # Create DataPortal object
    data = DataPortal()

    instance = model.create_instance(data)
    instance.display()
    
    results = opt.solve(instance, tee=True)
    instance.solutions.load_from(results)
    
    return instance    

def output_json(params, instance):
    
    # Calculate cost
    costs = [value(instance.gen_factor[i] * instance.x[i] * instance.gen_cost[i]) * -1 for i in range(1, 121)]
    costs[0] = costs[0] - value(instance.system_cost) * value(instance.x[1])
    
    # Calculate saving
    savings = [None] * 120
    for i in range(1, 121):
        temp_cons = value(instance.cons_day[i])
        temp_gen = value(instance.gen_factor[i]) * value(instance.x[i])
        if temp_cons < temp_gen:
            savings[i - 1] = temp_cons * value(instance.tou_dict[i]) + (temp_gen - temp_cons) * value(instance.nem)
        else:
            savings[i - 1] = temp_gen * value(instance.tou_dict[i])
          
    # Calculate incentive
    incentive = max(value(instance.incentive_per_kw) * value(instance.x[1]), value(instance.incentive_cap))
 
    # Reiterating the params from the API call
    data = {
        'station': params['station'],
        'biz': params['biz'],
        'roof': params['roof'],
        'wkrs': params['wkrs'],
        'sqft': params['sqft'],
        'wd_st': params['wd_st'],
        'wd_hrs': params['wd_hrs'],
        'we_st': params['we_st'],
        'we_hrs': params['we_hrs'],        
        'biz_input': params['biz_input'],
        'open_24_input': params['open_24_input'],
        'elec_heat_input': params['elec_heat_input'],
        'elec_cool_input': params['elec_cool_input'],
        'open_wkend_input': params['open_wkend_input'],
        'elec_cook_input': params['elec_cook_input'], 
        'refridg_input': params['refridg_input'], 
        'elec_mfg_input': params['elec_mfg_input'],
        "rec_cap": [value(instance.x[i]) for i in range(1, 121)], 
        "cons_day": [value(instance.cons_day[i]) for i in range(1, 121)], 
        "cons_night": [value(instance.cons_night[i]) for i in range(1, 121)],         
        "cons": [value(instance.cons_day[i]) + value(instance.cons_night[i]) for i in range(1, 121)], 
        "gen": [value(instance.gen_factor[i]) * value(instance.x[i]) for i in range(1, 121)], 
        "init_cost": value(instance.x[1]) * value(instance.system_cost) * -1, 
        "inspection_cost": [value(instance.inpect_cost[i]) for i in range(1, 121)],
        "cleaning_cost": [value(instance.clean_cost_per_kw[i]) * value(instance.x[i]) for i in range(1, 121)],
        "saving": savings, 
        "incentive": incentive
    }                            
                                 
    return json.dumps(data)

def run_model_api(params, 
                  file_path = file_path):
    model = create_model(station_id = int(params['station']), 
                         business_type = params['biz_input'], 
                         roof_sqft = float(params['roof']),
                         num_workers = int(params['wkrs']), 
                         sqft = float(params['sqft']),
                         start_year = start_year,
                         num_years = num_years,
                         wd_start_hr = int(params['wd_st']), 
                         wd_work_hrs = int(params['wd_hrs']),
                         we_start_hr = int(params['we_st']), 
                         we_work_hrs = int(params['we_hrs']),                            
                         opn24 = int(params['open_24_input']),
                         heat = int(params['elec_heat_input']), 
                         cool = int(params['elec_cool_input']),
                         opnwe = int(params['open_wkend_input']),
                         cook = int(params['elec_cook_input']), 
                         rfrd = int(params['refridg_input']),
                         manu = int(params['elec_mfg_input']), 
                         water = 0, 
                         elvr = 0, 
                         escl = 0, 
                         file_path = file_path)

    instance = run_model(model)  
    return output_json(params, instance)