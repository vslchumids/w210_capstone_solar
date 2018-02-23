## Data
* 724940_all.csv => 1 station all data with all columns
* 724940_daily_ts.csv => 1 station daily aggregated time series
* 724940_monthly_ts.csv => 1 station monthly aggregated time series
* NSRDB_CA_StationsMeta.csv => Meta data for CA weather stations
## Notebooks
* nsrdb_EDA.ipynb => EDA
* nsrdb_1station_sarima_daily.ipynb => SARIMA on daily data
* nsrdb_1station_sarima_weekly.ipynb => SARIMA on weekly data
* nsrdb_1station_sarima_monthly.ipynb =>  SARIMA on monthly data
* nsrdb_1station_LSTM_daily.ipynb => LSTM on daily data
* nsrdb_1station_LSTM_weekly.ipynb => LSTM on weekly data


## Scripts

* To download CA weather station data and untar
		* >> python get_data.py -c NSRDB_CA_StationsMeta.csv 

* To create a feature table based on CA weather station for clustering analysis
		* >> python build_yearly_features.py -i NSRDB_CA_StationsMeta.csv -o feature_table.csv

Note: The yearly feature is based on 2005 data and aggregated use average
