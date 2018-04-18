import numpy as np
import pandas as pd

#file_path = "../Data/"
file_path = "data/"

# Functions for getting incentives

def get_incentives_raw(file_path = file_path):        
    incent = pd.read_csv(file_path + "incentives.csv")
    incent['Incentive_Per_KW'] = incent['Incentive_Per_Watt'] * 1000.
    
    return incent

def get_incentives(file_path = file_path):
    incent = get_incentives_raw(file_path)
    
    incent_aggr = incent[['Nearest_Station', 'Incentive_Per_KW', 'Dollar_Cap']]
    incent_aggr = incent.groupby(['Nearest_Station']).mean()
    
    return incent_aggr

def get_incentive_at_station(station_id = 690150, 
                             file_path = file_path):
    try:
        incent = get_incentives(file_path)
        return incent.loc[station_id]['Incentive_Per_Watt'], incent.loc[station_id]['Dollar_Cap']
    except:
        return None, None
    
def main():
     print "Hello from incentive.py"