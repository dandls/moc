import subprocess
import click
import time 
import os
import pandas as pd

@click.command()
@click.option('--model', help = 'either neuralnet or logreg')
def benchmark(model):
    ids = range(1,11)
    datasets = os.listdir("../../saved_objects/data/") 
    datasets = ["boston", "diabetes", "ilpd", "kc2", "no2","pc1"]
    procs = []
    for data in datasets:
        for idx in ids:
            proc = subprocess.Popen(["venv-dice/bin/python3","cf-dice.py","--ncf", "10", "--openmlid", data, "--xorig", str(idx), "--model", "logreg"])   
            procs.append(proc)
    time.sleep(3600)
    for proc in procs:
        proc.kill()
    combine(model)

def combine(model):
    ids = range(1,11)
    if model == "logreg":
        datasets = os.listdir("../../saved_objects/data/") 
    else: 
        datasets = ["boston", "diabetes", "ilpd", "kc2", "no2","pc1"]
    for data in datasets:
        cf_list = [] 
        for idx in ids:
            csv_file = "cf-dice-" + model + "-" + str(idx) + ".csv"
            fname = os.path.join("../../saved_objects/data/", data, csv_file)
            if os.path.exists(fname):
                dat = pd.read_csv(fname, sep = ",")
                cf_list.append(dat)
        if len(cf_list) > 0:
            res = pd.concat(cf_list)
            out_file = os.path.join("../../saved_objects/data/", data, "cf-dice-" + model + ".csv")
            res.to_csv(out_file, index=False)


if __name__=="__main__":
    benchmark()
