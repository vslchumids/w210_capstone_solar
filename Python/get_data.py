import pandas as pd
import numpy as np
import itertools
import sys
import re
import os
import subprocess
import math
from argparse import ArgumentParser

def main(meta_table):
    stations_CA = pd.read_csv(meta_table)
    station_list = list(stations_CA['USAF'])
    print "There are total of {:d} stations".format(len(station_list))

    # Download Station files
    get_files = ['curl https://www1.ncdc.noaa.gov/pub/data/nsrdb-solar/solar-only/' + str(i) + '.tar.gz' + \
                 ' -o ' + str(i) + '.tar.gz -s' for i in station_list]
    print "Downloading station data ... "
    for i in get_files:
        print i
        os.system(i)
    print "Download Completed!!"

    downloaded_files = []
    for file in os.listdir('.'):
        if file.endswith('.tar.gz'):
            downloaded_files.append(file)
    print "Downloaded {:d} stations".format(len(downloaded_files))

    # Untar files
    for file in downloaded_files:
        subprocess.call(['tar', 'xvf', file])

## How to run
## python get_data.py -c NSRDB_CA_StationsMeta.csv
if __name__ == '__main__':

    # Define the argument parser
    parser = ArgumentParser(description="Download CA Station data based on station table")
    parser.add_argument("-c", "--conf",
                        dest="station_table",
                        default='NSRDB_CA_StationsMeta.csv',
                        help="California Station Desc Table")
    args = parser.parse_args()
    main(args.station_table)
