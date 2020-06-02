import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from recourse import action_set
from recourse import flipset
import click
import os
import json
import tensorflow as tf
from tensorflow import keras
import random

@click.command()
@click.option('--openmlid', help = 'ID of openml task')
@click.option('--ncf', default = 3, help = 'Number of counterfactuals to compute')
def counterfactual(openmlid,  ncf):
    if openmlid in ['cmc', 'tic-tac-toe', 'plasma_retinol', 'kr-vs-kp']:
        return Null
    random.seed(42)
    folder = "../../saved_objects/data"
    folder = os.path.join(folder, openmlid)
    data_path = os.path.join(folder, "data_encoded_refcat.csv")
    model_path = os.path.join(folder, "logreg.h5")
    feature_dic_path = os.path.join(folder, "feature_types.json")
    # File that contains the ids for which to compute counterfactuals
    cf_ids_infile = os.path.join(folder, "sampled_ids.txt")
    cf_outfile = os.path.join(folder, "cf-recourse-logreg.csv")

    df = pd.read_csv(data_path, sep = ",")
    with open(feature_dic_path, "r") as json_file:
    	feature_dic = json.load(json_file)
    features = df.columns
    outcome_name = feature_dic.get("target")
    features = [feature for feature in features if feature != outcome_name]

    y, X = df[outcome_name], df[features]
    
    with open(cf_ids_infile, "r") as f:
        ids = f.readlines()
    # Index starts at 0 in pandas dataframes
    ids = [int(i) - 1 for i in ids]
    if openmlid== "kr-vs-kp":
        cb = {f : [0,1] for f in features} 
        A = action_set.ActionSet(X, custom_bounds = cb)  ## matrix of features. ActionSet will learn default bounds and step-size.
    else:
        A = action_set.ActionSet(X)
    model = keras.models.load_model(model_path) 
    weights = model.get_weights()
    # Generate counterfactual examples
    prediction = model.predict(X.values).flatten()
    cf_dataframes = []
    for idx in ids:
        print(idx)
        w = weights[0].flatten()
        b = weights[1]
        # Explanations for counterfactual with desired class y=0
        if prediction[idx] > 0.5:
            w = -w
            b = -b
        A.align(coefficients = w, intercept = b)  ## tells `ActionSet` which directions each feature should move in to produce positive change.

        # build a flipset for one individual
        datapoint = X.iloc[idx]
        fs = flipset.Flipset(x = datapoint.get_values(), action_set = A, coefficients = w, intercept = b)
        fs.populate(enumeration_type = 'distinct_subsets', total_items = ncf)
        cf_df = fs.to_flat_df()
        cf_ids = np.unique(cf_df.index.values)
        for cfi in cf_ids:
            cf = cf_df.loc[[cfi]]
            x = datapoint.copy()
            for r in range(0,len(cf)):
                feature = cf.iloc[r]['features']
                x_new = cf.iloc[r]['x_new']
                x[feature] = x_new
                x['row_ids'] = int(idx) + 1
            cf_dataframes.append(x.to_frame().T)
   
    cf_df = pd.concat(cf_dataframes)
    # Rescale features to original scale
    numeric_features = [k for k, v in feature_dic.items() if v == 'numeric']

    dapath = os.path.join(folder, "data_orig.csv")
    dfo = pd.read_csv(dapath, sep = ",")
    for feat in numeric_features:
        cf_df[feat] = cf_df[feat] * (max(dfo[feat]) - min(dfo[feat])) +  min(dfo[feat])  
    cf_df.to_csv(cf_outfile, index = False)

if __name__=='__main__':
    counterfactual()
