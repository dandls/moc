# MOC - Multi-Objective Counterfactuals

This repository provides code and examples for generating multi-objective counterfactuals for the following paper:      
Dandl, S., Molnar, C., Binder, M., Bischl, B. (2020): Multi-Objective Counterfactual Expalantions. 

For all computations, we used either the statistical software R (version ≥ 3.4.4) or Python (version 3.6.9). 

> :bangbang: **There is now an additional branch of the counterfactuals package without dependency to the local version of the [iml package](https://github.com/susanne-207/moc/tree/master/iml)** :bangbang:   
> Branch: [moc_without_iml](https://github.com/susanne-207/moc/tree/moc_without_iml)   
> You can load this package with 
> `remotes::install_git(url = "https://github.com/susanne-207/moc", branch = "moc_without_iml", subdir = "counterfactuals")`.
> Use this branch if you do not want to reproduce the paper but want to run MOC with your own examples. 

## Overview 

* **Code to reproduce analysis done in the Paper**:
    * *credit_example*: Example R code that generates counterfactuals on the German credit dataset, as used in the Paper. 
    * *appendix_irace*: Code that was used to run [iterated F-racing](https://doi.org/10.1016/j.orp.2016.09.002) to tune the hyperparameters of MOC. Includes a `Makefile`. 
    * *benchmark*: Code that was used to generate the benchmark data. Includes a `Makefile`.
    * *benchmark_analysis*: R code for the analysis of the benchmark results.
    * *helpers*: Helper functions.
    * *saved_objects*: Saved benchmark and irace results to duplicate results without the necessity to rerun experiments. 
* **Package Code**:
    * *iml*: Copy of the [`iml` Package](https://github.com/christophM/iml). This is code from the ["`conditional`" branch as of February 2020](https://github.com/christophM/iml/tree/c12febbfaee07ccb2c8bac025d9faf0045ee178f) with minor fixes that will eventually be merged into `iml`.
    * *counterfactuals*: [`counterfactuals`](counterfactuals/) package in an early state. To be released as an R package in the future.

## Manual 

### Download the github repository 

```
git clone https://github.com/susanne-207/moc.git
``` 

### Statistical Analysis
For the German Credit dataset example shown in the paper, step through this file:
[german_credit_application.R](credit_example/german_credit_application.R) 

For the results of the benchmark study, step through the following file:
[evaluate_cfexps.R](benchmark_analysis/evaluate_cfexps.R) 

### `irace` run
Have a look on the [Makefile](appendix_irace/Makefile).

`make train-models` will train the classification models for iterated racing on the tasks derived from OpenML.

`make get-evals` will return the number of generations to ensure convergence of the hypervolume in most cases for running MOC within iterated F-racing. 
	
`make run-irace` this will start iterated F-racing using the maximum number of generations and the trained models from the steps before. 

`make get-generations` will return the number of generations necessary to ensure convergence AFTER the other parameters were 
tuned. 

All results are saved in a new folder called *saved_objects_rerun*. 

### Rerun Benchmark
Have a look on the [Makefile](benchmark/Makefile).

`make train-models` will train the classification models for the benchmark on the tasks derived from OpenML. The id of the tasks are saved in [benchmark_task_ids.rds](helpers/benchmark_task_ids.rds).
The models are saved in *saved_objects_rerun*.

`make run-moc` will run the benchmark for MOC. 

`make run-pair` will run the benchmark for Pair.

`make run-tweaking` will run the benchmark for Tweaking. 

Recourse and DiCE have a seperate [Makefile](benchmark/python_recoures_dice/Makefile) since they are Python and not R based.
At first a virtual environment is necessary using `make venv-dice` and `make venv-recourse`. 
To run the experiments, use `make all`. 
