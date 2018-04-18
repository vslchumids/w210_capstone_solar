from pyomo.environ import *
from pyomo.opt import SolverFactory

from flask import Flask
from flask import request
import json
import date_time_util
import pandas as pd

from optimization import run_model_api, create_model, run_model, system_costs, gen_costs, day_nem_tou, gen_kwh, gen_kwh_by_month_ind

#########################################################################################
# Globals
#########################################################################################

header_text = '''
    <html>\n<head> <title>SolaRise</title> </head>\n<body>'''
instructions = '''
    <p><Welcome to SolaRise</p>
    <p><em>Hint</em>: Pass in your name as a parameter to see a hello message: /?name=<your name></p>\n'''
home_link = '<p><a href="/">Back</a></p>\n'
solarise_link = '<p><a href="/solarise">Solar Model</a></p>\n'
footer_text = '</body>\n</html>'
image_link = '<img src="https://i.pinimg.com/originals/6f/22/00/6f2200e9c33ab0437a3ddf4d17961b68.png" alt="Line1" width="300" height="300">'

#########################################################################################
# Functions
#########################################################################################

# Map business type values passed to the api from the R Shint front end
# to business type inputs to the consumption model
def map_biz_type_to_model_input(biz, refridg):
    model_biz_type = None

    if biz in ("Office", "ofc"):
        model_biz_type = "Office"
    elif biz in ("Laboratory", "lab"):
        model_biz_type = "Laboratory"        
    elif biz in ("Education", "edu"):
        model_biz_type = "Education"        
    elif biz in ("Lodging", "lodge"):
        model_biz_type = "Lodging"        
    elif biz in ("Service", "service"):
        model_biz_type = "Service"        
    elif biz in ("Other", "oth"):
        model_biz_type = "Other"
    elif biz in ("Warehouse", "war"):
        if refridg == "Y":
            model_biz_type = "Refrigerated warehouse"
        else:
            model_biz_type = "Nonrefrigerated warehouse"
    elif biz in ("Restuarant", "rest"):
        model_biz_type = "Food service"
    elif biz in ("ConvenienceStore", "conv"):
        model_biz_type = "Food sales"
    elif biz in ("ReligiousWorship", "reli"):
        model_biz_type = "Public assembly" 
    elif biz in ("OutpatientHealth", "out_health"):
        model_biz_type = "Outpatient health care"
    elif biz in ("InpatientHealth", "in_health"):
        model_biz_type = "Inpatient health care"
    elif biz in ("NursingHome", "nurse"):
        model_biz_type = "Nursing"
    elif biz in ("StripMall", "strip"):
        model_biz_type = "Strip shopping mall"
    elif biz in ("EnclosedMall", "enc_mall"):
        model_biz_type = "Enclosed mall"
    elif biz in ("RetailOther", "ret_oth"):
        model_biz_type = "Retail other than mall"
    elif biz in ("PublicAssembly"): 
        model_biz_type = "Public assembly"
    else:
        model_biz_type = "Other"

    return model_biz_type

# Get parameters passed to the solarise api hosted at 
# http://solarise.7bsuxv7th2.us-west-2.elasticbeanstalk.com/solarise?
# 
# Parameters:
#   station: [required] Weather Station (derived from Address in R Shiny layer)
#   biz: [required] Business Type (please remove spaces between words)
#   roof: [required] Roof Size (Sq ft)
#   wkrs: [required] # Employees
#   sqft: [required] office Size (Sq ft)
#   wd_st: Weekday Start Hours
#   wd_hrs: Weekday Daily Hours
#   we_st: Weekend Start Hours
#   we_hrs: Weekend Daily Hours
#   open_24: Open 24 (=Y if checked; omitted if unchecked)
#   elec_heat: Electric Heat (=Y if checked; omitted if unchecked)
#   elec_cool: Electric Cool (=Y if checked; omitted if unchecked)
#   open_wkend: Open Weekend (=Y if checked; omitted if unchecked)
#   elec_cook: Eectric Cook (=Y if checked; omitted if unchecked)
#   refridg: Refridgeration (=Y if checked; omitted if unchecked)
#   elec_mfg: Electic Manufacture (=Y if checked; omitted if unchecked)
def get_solarise_params():
    params = {}

    if 'station' in request.args:
        params['station'] = request.args['station']
    else:
        params['station'] = None

    if 'biz' in request.args:
        params['biz'] = request.args['biz']
    else:
        params['biz'] = None        

    if 'roof' in request.args:
        params['roof'] = request.args['roof']
    else:
        params['roof'] = None

    if 'wkrs' in request.args:
        params['wkrs'] = request.args['wkrs']
    else:
        params['wkrs'] = None

    if 'sqft' in request.args:
        params['sqft'] = request.args['sqft']
    else:
        params['sqft'] = None

    if 'wd_st' in request.args:
        params['wd_st'] = request.args['wd_st']
    else:
        params['wd_st'] = None

    if 'wd_hrs' in request.args:
        params['wd_hrs'] = request.args['wd_hrs']
    else:
        params['wd_hrs'] = None

    if 'we_st' in request.args:
        params['we_st'] = request.args['we_st']
    else:
        params['we_st'] = None

    if 'we_hrs' in request.args:
        params['we_hrs'] = request.args['we_hrs']
    else:
        params['we_hrs'] = None

    if 'open_24' in request.args:
        params['open_24'] = request.args['open_24']
    else:
        params['open_24'] = None

    if 'elec_heat' in request.args:
        params['elec_heat'] = request.args['elec_heat']
    else:
        params['elec_heat'] = None

    if 'elec_cool' in request.args:
        params['elec_cool'] = request.args['elec_cool']
    else:
        params['elec_cool'] = None

    if 'open_wkend' in request.args:
        params['open_wkend'] = request.args['open_wkend']
    else:
        params['open_wkend'] = None 

    if 'elec_cook' in request.args:
        params['elec_cook'] = request.args['elec_cook']
    else:
        params['elec_cook'] = None

    if 'refridg' in request.args:
        params['refridg'] = request.args['refridg']
    else:
        params['refridg'] = None
        
    if 'elec_mfg' in request.args:
        params['elec_mfg'] = request.args['elec_mfg']
    else:
        params['elec_mfg'] = None

    return params

# Transform the checkbox parameter values to binaries
def transform_checkbox_param_to_binary(checkbox_param):
    if checkbox_param == 'Y':
        return 1
    else:
        return 0

# Transform parameters passed to the solarise api
def get_transformed_solarise_params():
    params = get_solarise_params()

    # Get and transform the business type parameter at the end.
    # Need value of the refridgeration parameter in case the 
    # business is a warehouse   
    params['biz_input'] = map_biz_type_to_model_input(params['biz'], params['refridg'])
    params['open_24_input'] = transform_checkbox_param_to_binary(params['open_24'])
    params['elec_heat_input'] = transform_checkbox_param_to_binary(params['elec_heat'])
    params['elec_cool_input'] = transform_checkbox_param_to_binary(params['elec_cool'])
    params['open_wkend_input'] = transform_checkbox_param_to_binary(params['open_wkend'])
    params['elec_cook_input'] = transform_checkbox_param_to_binary(params['elec_cook'])
    params['refridg_input'] = transform_checkbox_param_to_binary(params['refridg'])
    params['elec_mfg_input'] = transform_checkbox_param_to_binary(params['elec_mfg'])

    return params

# Run optimization model and return JSON with model results
def solar_rec(params):
    json_output = run_model_api(params)
    return json_output

# Print a hello greeting
def say_hello(username = "World"):
    return '<p>Hello %s!</p>\n' % username

#########################################################################################
# API Declarations
#########################################################################################

# EB looks for an 'application' callable by default.
application = Flask(__name__)

# Rule for the index page (Hello World demo)
@application.route('/')
def api_index():
    if 'name' in request.args:
        return header_text + say_hello(request.args['name']) + home_link + footer_text
    else:    
        return header_text + instructions + solarise_link + footer_text

# Rule for the solarise page
@application.route('/solarise')
def api_solarise():
    params = get_transformed_solarise_params() 
    return solar_rec(params)

#########################################################################################
# Main
#########################################################################################

# Run the app
if __name__ == "__main__":
    # Setting debug to True enables debug output. This line should be
    # removed before deploying a production app.
    application.debug = True
    application.run()
