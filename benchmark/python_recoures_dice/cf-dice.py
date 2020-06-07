import csv
import DiCE.dice_ml as dice_ml
from DiCE.dice_ml.utils import helpers # helper functions
import pandas as pd
import os
import tensorflow as tf
import json
import click
import re
import numpy as np
from datetime import datetime
from tensorflow import keras

@click.command()
@click.option('--openmlid', type=str, help = 'ID of openml task')
@click.option('--model', type=str, help = 'either neuralnet or logreg')
@click.option('--ncf', type=int, help = 'Number of counterfactuals to compute')
def counterfactual(openmlid, model, ncf):
    folder = os.path.join("../../saved_objects_rerun/benchmark/", openmlid)
    # For the logistic regression, binary features are not one-hot encoded
    # This is necessary because recourse currently does not work with one-hot encoded features
    data_path = os.path.join(folder, "data_orig.csv")
    model_path = os.path.join(folder, model + ".h5")
    feature_dic_path = os.path.join(folder, "feature_types.json")
    # File that contains the ids for which to compute counterfactuals
    cf_ids_infile = os.path.join(folder, "sampled_ids.txt")
    with open(cf_ids_infile, "r") as f:
        ids = f.readlines()
    # Index starts at 0 in pandas dataframes
    ids = [int(i) - 1 for i in ids]
    cf_outfile = os.path.join(folder, "cf-dice-" + model + ".csv")
    full_df = pd.read_csv(data_path, sep = ",")
    with open(feature_dic_path, "r") as json_file:
        feature_dic = json.load(json_file)
    features = feature_dic.values()
    feature_types = feature_dic.get("type")
    numeric_features = [k for k, v in feature_dic.items() if v == 'numeric']
    outcome_name = feature_dic.get("target")
    if model == "logreg":
        data_path_enc = os.path.join(folder, "data_encoded_refcat.csv")
        df_enc = pd.read_csv(data_path_enc, sep = ",")
        fnames = df_enc.columns
        encoded_features = [f for f in fnames if f not in numeric_features]
        full_df = pd.concat([full_df[numeric_features], df_enc[encoded_features]], axis=1)
        # Bring columns into original order again
        full_df = full_df[fnames]
        numeric_features = full_df.columns
        numeric_features = [nf for nf in numeric_features if not nf == outcome_name]

    df = full_df.copy()
    df = df.drop(ids)
    d = dice_ml.Data(dataframe=df.copy(),
            continuous_features=numeric_features,
            outcome_name=outcome_name)

    # There is a problem with the feature scaling by the inverse median absolute deviation in DiCE
    # For e.g. features with mostly 0's and few non zeros, it becomes zero and DiCE returns empty counterfactuals
    mads = list(d.get_mads().values())
    mad_zero_flag = any(mad  == 0 for mad in mads)
    if mad_zero_flag:
        print("Found problem with MAD")
        inverse_normalized_mads = [round(1/get_mad(df[feat]),2) for feat in numeric_features]
        feature_weights = {k:v for k,v  in zip(numeric_features, inverse_normalized_mads)}
        one_hot_feature_names = list(set(d.encoded_feature_names) - set(numeric_features))
        # One-hot encoded feature columns get weight 1.0 by default
        cat_weights = {k:1.0 for k in one_hot_feature_names}
        feature_weights = {**feature_weights, **cat_weights}
    m = dice_ml.Model(model_path=model_path)
    # DiCE explanation instance
    exp = dice_ml.Dice(d,m)
    # Generate counterfactual examples
    cf_dataframes = []
    for idx in ids:
        instance = full_df.copy().iloc[idx].to_dict()
        # The desired prediction is the opposite of the *predicted* outcome
        query_instance = d.prepare_query_instance(query_instance=instance, encode=True)
        query_instance = np.array([query_instance.iloc[0].values])
        prediction = exp.predict_fn(query_instance)[0][0]
        if prediction > 0.5:
            dclass = 0
        else:
            dclass = 1
        if mad_zero_flag:
            dice_exp = exp.generate_counterfactuals(instance, total_CFs=ncf, desired_class=dclass,
                feature_weights = feature_weights, verbose = True)
        else:
            dice_exp = exp.generate_counterfactuals(instance, total_CFs=ncf, desired_class=dclass, verbose = True)
        dice_cfs = dice_exp.final_cfs_df
        dice_cfs['row_ids'] = idx + 1
        cf_dataframes.append(dice_cfs)
        date_obj = datetime.now()
        date_string = date_obj.strftime('%Y/%m/%d-%H:%M:%S')
        print(date_string + ' | row: ' + str(idx + 1) + ' | dataset: ' + openmlid + ' | model: ' + model)

    cf_df = pd.concat(cf_dataframes)
    cf_df.to_csv(cf_outfile, index = False)


def get_mad(x, fallback = 0.1):
    '''
    Computes the mean absolute deviation for vector x
    Returns 0.1 (fallback) if mad is zero, because later the inverse mad is used
    '''
    x = standardize(x)
    mad = np.median(abs(x - np.median(x)))
    return fallback if mad==0 else mad

def standardize(x):
    '''
    Map feature to [0,1] interval
    '''
    return (x - min(x))/(max(x) - min(x))

if __name__=='__main__':
    counterfactual()
