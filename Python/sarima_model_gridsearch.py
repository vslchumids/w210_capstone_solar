import pandas as pd
import numpy as np
import itertools
import sys
import re
import os
import subprocess
import math
import itertools
import warnings
from argparse import ArgumentParser
from configparser import ConfigParser

# TSA from Statsmodels
import statsmodels.api as sm
import statsmodels.formula.api as smf
import statsmodels.tsa.api as smt


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

def parse_configs(configs_file, main_section='sarima_params'):
    configs = ConfigParser()
    configs.read(configs_file)
    main_params = dict(configs[main_section])
    for k, v in main_params.items():
        main_params[k] = int(main_params[k])
    return main_params


# Model status
def model_resid_stats(model_results,
                      het_method='breakvar',
                      norm_method='jarquebera',
                      sercor_method='ljungbox',
                      verbose=True,
                      ):
    # Re-run the ARIMA model statistical tests, and more. To be used when selecting viable models.
    (het_stat, het_p) = model_results.test_heteroskedasticity(het_method)[0]
    norm_stat, norm_p, skew, kurtosis = model_results.test_normality(norm_method)[0]
    sercor_stat, sercor_p = model_results.test_serial_correlation(method=sercor_method)[0]
    sercor_stat = sercor_stat[-1] # last number for the largest lag
    sercor_p = sercor_p[-1] # last number for the largest lag

    # Run Durbin-Watson test on the standardized residuals.
    # The statistic is approximately equal to 2*(1-r), where r is the sample autocorrelation of the residuals.
    # Thus, for r == 0, indicating no serial correlation, the test statistic equals 2.
    # This statistic will always be between 0 and 4. The closer to 0 the statistic,
    # the more evidence for positive serial correlation. The closer to 4,
    # the more evidence for negative serial correlation.
    # Essentially, below 1 or above 3 is bad.
    dw_stat = sm.stats.stattools.durbin_watson(model_results.filter_results.standardized_forecasts_error[0, model_results.loglikelihood_burn:])

    # check whether roots are outside the unit circle (we want them to be);
    # will be True when AR is not used (i.e., AR order = 0)
    arroots_outside_unit_circle = np.all(np.abs(model_results.arroots) > 1)
    # will be True when MA is not used (i.e., MA order = 0)
    maroots_outside_unit_circle = np.all(np.abs(model_results.maroots) > 1)
    
    if verbose:
        print('Test heteroskedasticity of residuals ({}): stat={:.3f}, p={:.3f}'.format(het_method, het_stat, het_p));
        print('\nTest normality of residuals ({}): stat={:.3f}, p={:.3f}'.format(norm_method, norm_stat, norm_p));
        print('\nTest serial correlation of residuals ({}): stat={:.3f}, p={:.3f}'.format(sercor_method, sercor_stat, sercor_p));
        print('\nDurbin-Watson test on residuals: d={:.2f}\n\t(NB: 2 means no serial correlation, 0=pos, 4=neg)'.format(dw_stat))
        print('\nTest for all AR roots outside unit circle (>1): {}'.format(arroots_outside_unit_circle))
        print('\nTest for all MA roots outside unit circle (>1): {}'.format(maroots_outside_unit_circle))
    
    stat = {'het_method': het_method,
            'het_stat': het_stat,
            'het_p': het_p,
            'norm_method': norm_method,
            'norm_stat': norm_stat,
            'norm_p': norm_p,
            'skew': skew,
            'kurtosis': kurtosis,
            'sercor_method': sercor_method,
            'sercor_stat': sercor_stat,
            'sercor_p': sercor_p,
            'dw_stat': dw_stat,
            'arroots_outside_unit_circle': arroots_outside_unit_circle,
            'maroots_outside_unit_circle': maroots_outside_unit_circle,
            }
    return stat

def sarima_model_assumptions(sarima_best):
    
    # Define testing Methods
    het_method='breakvar'
    norm_method='jarquebera'
    sercor_method='ljungbox'

    # Run test
    het_stat, het_p = sarima_best.test_heteroskedasticity(het_method)[0]
    norm_stat, norm_p, skew, kurtosis = sarima_best.test_normality(norm_method)[0]
    sercor_stat, sercor_p = sarima_best.test_serial_correlation(method=sercor_method)[0]
    sercor_stat = sercor_stat[-1] # last number for the largest lag
    sercor_p = sercor_p[-1] # last number for the largest lag

    # check whether roots are outside the unit circle (we want them to be);
    # will be True when AR is not used (i.e., AR order = 0)
    arroots_outside_unit_circle = np.all(np.abs(sarima_best.arroots) > 1)
    # will be True when MA is not used (i.e., MA order = 0)
    maroots_outside_unit_circle = np.all(np.abs(sarima_best.maroots) > 1)

    # display results
    print('Test SARIMA model assuptions on residuals: selected model should meet all assumptions')
    print('*** Heteroskedastcity (Null: variance is not increasing or homostedasticity) ***')
    print('Test heteroskedasticity of residuals ({}): stat={:.3f}, p={:.3f}'.format(het_method, het_stat, het_p))
    print('\n*** Normality with Jarqeue-Bera (Null: normally distributed) ***')
    print('Test normality of residuals ({}): stat={:.3f}, p={:.3f}'.format(norm_method, norm_stat, norm_p));
    print('\n*** Serial Autocorrelation with Ljung-box (Null: there is no serial autocorrelation) ***')
    print('Test serial correlation of residuals ({}): stat={:.3f}, p={:.3f}'.format(sercor_method, sercor_stat, sercor_p));
    print('\n*** Stationary ***')
    print('Test for all AR roots outside unit circle (>1): {}'.format(arroots_outside_unit_circle))
    print('\n*** Invertability ***')
    print('\nTest for all MA roots outside unit circle (>1): {}'.format(maroots_outside_unit_circle))

def model_gridsearch(ts,
                     p_min,
                     d_min,
                     q_min,
                     p_max,
                     d_max,
                     q_max,
                     sP_min,
                     sD_min,
                     sQ_min,
                     sP_max,
                     sD_max,
                     sQ_max,
                     trends,
                     s=None,
                     enforce_stationarity=True,
                     enforce_invertibility=True,
                     simple_differencing=False,
                     plot_diagnostics=False,
                     verbose=False,
                     filter_warnings=True,
                    ):
    '''Run grid search of SARIMAX models and save results.
    '''
    
    cols = ['p', 'd', 'q', 'sP', 'sD', 'sQ', 's', 'trend',
            'enforce_stationarity', 'enforce_invertibility', 'simple_differencing',
            'aic', 'bic',
            'het_p', 'norm_p', 'sercor_p', 'dw_stat',
            'arroots_gt_1', 'maroots_gt_1',
            'datetime_run']

    # Initialize a DataFrame to store the results
    df_results = pd.DataFrame(columns=cols)

    mod_num=0
    for trend,p,d,q,sP,sD,sQ in itertools.product(trends,
                                                  range(p_min,p_max+1),
                                                  range(d_min,d_max+1),
                                                  range(q_min,q_max+1),
                                                  range(sP_min,sP_max+1),
                                                  range(sD_min,sD_max+1),
                                                  range(sQ_min,sQ_max+1),
                                                  ):
        # initialize to store results for this parameter set
        this_model = pd.DataFrame(index=[mod_num], columns=cols)


        if p==0 and d==0 and q==0:
            continue

        try:
            print "Evaluating SARIMA model ({:d}, {:d}, {:d})({:d}, {:d}, {:d}, {:d}) ...".format(p, d, q, sP, sD, sQ, s)
            model = sm.tsa.SARIMAX(ts,
                                   trend=trend,
                                   order=(p, d, q),
                                   seasonal_order=(sP, sD, sQ, s),
                                   enforce_stationarity=enforce_stationarity,
                                   enforce_invertibility=enforce_invertibility,
                                   simple_differencing=simple_differencing,
                                  )
            
            if filter_warnings is True:
                with warnings.catch_warnings():
                    warnings.filterwarnings("ignore")
                    model_results = model.fit(disp=0)
            else:
                model_results = model.fit()

            if verbose:
                model_results.summary()

            if plot_diagnostics:
                model_results.plot_diagnostics();

            stat = model_resid_stats(model_results,
                                     verbose=verbose)

            this_model.loc[mod_num, 'p'] = p
            this_model.loc[mod_num, 'd'] = d
            this_model.loc[mod_num, 'q'] = q
            this_model.loc[mod_num, 'sP'] = sP
            this_model.loc[mod_num, 'sD'] = sD
            this_model.loc[mod_num, 'sQ'] = sQ
            this_model.loc[mod_num, 's'] = s
            this_model.loc[mod_num, 'trend'] = trend
            this_model.loc[mod_num, 'enforce_stationarity'] = enforce_stationarity
            this_model.loc[mod_num, 'enforce_invertibility'] = enforce_invertibility
            this_model.loc[mod_num, 'simple_differencing'] = simple_differencing
            this_model.loc[mod_num, 'aic'] = model_results.aic
            this_model.loc[mod_num, 'bic'] = model_results.bic
            this_model.loc[mod_num, 'het_p'] = stat['het_p']
            this_model.loc[mod_num, 'norm_p'] = stat['norm_p']
            this_model.loc[mod_num, 'sercor_p'] = stat['sercor_p']
            this_model.loc[mod_num, 'dw_stat'] = stat['dw_stat']
            this_model.loc[mod_num, 'arroots_gt_1'] = stat['arroots_outside_unit_circle']
            this_model.loc[mod_num, 'maroots_gt_1'] = stat['maroots_outside_unit_circle']
            this_model.loc[mod_num, 'datetime_run'] = pd.to_datetime('today').strftime('%Y-%m-%d %H:%M:%S')

            df_results = df_results.append(this_model)
            mod_num+=1
        except:
            continue
    return df_results

def model_performance(y, y_hat):
    mse = np.mean((y - y_hat)**2)
    perc_err = (100.0*(y - y_hat))/y
    abs_err = abs(y - y_hat) * 1.0
    dsum=sum(abs(y[1:] - y_hat[1:]))
    t = len(y)
    denom = (1.0/(t - 1))* dsum
    return np.sqrt(mse), np.mean(abs(perc_err)), np.mean(abs_err/denom)

def main(station_table, metric, p_min = 0, d_min = 0, q_min = 0,  
         p_max = 2, d_max = 1, q_max = 2, sp_min = 0,  sd_min = 0,
         sq_min = 0, sp_max = 1, sd_max = 1, sq_max = 1, s=52):
    """
    Parameters
    ----------
    station_name : str
    metric : str
    p_min : int
    d_min : int
    q_min : int
    p_max : int
    d_max : int
    q_max : int
    sp_min : int
    sd_min : int
    sq_min : int
    sp_max : int
    sd_max : int
    sq_max : int
    s : int
    """
    
    station_name = station_table.split('.')[0]
    model_ranking = station_name + '_model_ranking.csv'
    
    # Load California station meta table and clean up
    print "Processing {:s}".format(station_table)
    dataset = pd.read_csv(station_table)
    dataset_post = preprocess(dataset)
    dataset_post['YYYY-MM-DD'] = pd.to_datetime(dataset_post['YYYY-MM-DD'])  
    dataset_post = dataset_post.rename({'YYYY-MM-DD':'Datetime'}, axis = 1)
    
    # Create weekly average table
    daily = dataset_post.groupby('Datetime')
    daily_ts = daily['Datetime', 'GHI'].mean()
    weekly_ts = daily_ts.resample('W').mean()

    # Create train and test data
    print "Train Test Split ..."
    weekly_ts_short = weekly_ts.loc['2001':'2005']
    n_sample = weekly_ts_short.shape[0]
    n_train = int(0.95*n_sample) + 1
    n_forecast=n_sample-n_train
    ts_train = weekly_ts_short.iloc[:n_train]['GHI']
    ts_test = weekly_ts_short.iloc[n_train:]['GHI']
    print "Training set size: {:s}".format(ts_train.shape)
    print "Test set size {:s}".format(ts_test.shape)

    # Model grid search
    print "Model Grid Search ... "
    trends=['n']

    df_results = model_gridsearch(weekly_ts_short['GHI'],
                                  p_min,
                                  d_min,
                                  q_min,
                                  p_max,
                                  d_max,
                                  q_max,
                                  sp_min,
                                  sd_min,
                                  sq_min,
                                  sp_max,
                                  sd_max,
                                  sq_max,
                                  trends,
                                  s=s,
                                  enforce_stationarity=True,
                                  enforce_invertibility=True,
                                  simple_differencing=False,
                                  plot_diagnostics=False,
                                  verbose=False,
                                  )
    print "Total {:d} models evaluated".format(df_results.shape[0])
    df_results.to_csv(model_ranking)
    
    # Select the best model
    models = pd.read_csv(model_ranking)
    models = models.sort_values(by=metric)
    p, d, q, sp, sd, sq, s = list(models.iloc[0, 1:8])
    print "The best model is SARIMA({:d}, {:d}, {:d})({:d}, {:d}, {:d}, {:d})".format(p, d, q, sp, sd, sq, s)
    
    # Create the best model and test assumptions
    mod = sm.tsa.statespace.SARIMAX(ts_train, order=(p,d,q), seasonal_order=(sp,sd,sq,s), simple_differencing=False)
    print "\nFitting the model ..."
    sarima_best = mod.fit()
    print "\nModel summary"
    print(sarima_best.summary())
    sarima_model_assumptions(sarima_best)
    
    ## Model performance
    pred_begin = ts_train.index[s+1]
    pred_end = ts_test.index[-1]
    pred = sarima_best.get_prediction(start= pred_begin.strftime('%Y-%m-%d'), end= pred_end.strftime('%Y-%m-%d'))
    pred_mean = pred.predicted_mean
          
    # Insample fit
    rmse, mape, mase = model_performance(ts_train[s+1:], pred_mean.loc[ts_train.index[s+1:]])
    print "Model Insample Fit"
    print("Root Mean Square Error: {:.3f}".format(rmse))
    print("Mean Absolute Percentage Error: {:.3f}%".format(mape))
    print("Mean Absolute Scale Error: {:.3f}".format(mase))
          
    # Holdout prediction fit
    rmse, mape, mase = model_performance(ts_test, pred_mean.loc[ts_test.index])
    print "Model Prediction Fit"
    print("Root Mean Square Error: {:.3f}".format(rmse))
    print("Mean Absolute Percentage Error: {:.3f}%".format(mape))
    print("Mean Absolute Scale Error: {:.3f}".format(mase))
    
    # Save model
    saved_model = station_name + '.p'
    print "Save model {:s}...".format(saved_model)
    sarima_best.save(saved_model)

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
    parser.add_argument("-c", "--config-file",
                        dest="config_file",
                        default='config.ini',
                        help="Config File (default: config_sarima.ini in current folder")
    parser.add_argument("-m", "--metric",
                    dest="metric",
                    default='bic',
                    help="Metric to rank and select the best model")
    args = parser.parse_args()
    sarima_params = parse_configs(args.config_file)
    main(args.station_table, args.metric, **sarima_params)