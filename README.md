# MOC - Multi-Objective Counterfactuals

This repository provides code and examples for generating multi-objective counterfactuals for the following paper:      
Dandl, S., Molnar, C., Binder, M., Bischl, B. (2020): Multi-Objective Counterfactual Expalantions. 

For all computations, we used either the statistical software R (version ≥ 3.4.4) or Python (version 3.6.9)

## Overview 

* *appendix_irace_rerun*: Code to rerun the iterated F-racing to tune the hyperparameters of MOC. Includes a `Makefile`. 
* *appendix_study_duplicate*: R code to duplicate distribution consistency study. 
* *benchmark_duplicate*: R code to duplicate benchmark study. Includes a `Makefile`. 
* *examples_duplicate*: R code to duplicate German credit dataset example. 
* *helpers*: Helper functions. 
* *iml*: R code of the [iml Package](https://github.com/christophM/iml) (copy of November 2019) in addition to the code for [Counterfactual Explanations](https://github.com/susanne-207/moc/blob/master/iml/R/Counterfactuals.R). 
* *saved_objects*: Saved benchmark and irace results to duplicate results without the necessity to rerun the experiments. 



## Manual 

### Download the github repository 

```
git clone https://github.com/susanne-207/moc.git
``` 
### Duplicate results
For the results of the benchmark study, step through the following file:      
[evaluate_cfexps.R](https://github.com/susanne-207/moc/blob/master/benchmark_duplicate/evaluate_cfexps.R)    

For the results of the German Credit dataset example, step through this file:     
[german_credit_application.R](https://github.com/susanne-207/moc/blob/master/examples_duplicate/german_credit_application.R) 

For the distribution consistency study of the appendix, step through:     
[distribution_consistency_study.R](https://github.com/susanne-207/moc/blob/master/appendix_study_duplicate/distribution_consistency_study.R)

### Rerun irace 
Have a look on the [Makefile](https://github.com/susanne-207/moc/blob/master/appendix_irace_rerun/Makefile).

`make train-models` will train the classification models for iterated racing on the tasks derived from OpenML.    

`make get-evals` will return the number of generations to ensure convergence of the hypervolume in most cases for running MOC within iterated F-racing. 
	
`make run-irace` this will start iterated F-racing using the maximum number of generations and the trained models from the steps before. 

`make get-generations` will return the number of generations necessayr to ensure convergence AFTER the other parameters were 
tuned. 

All results are saved in a new folder called *saved_objects_rerun*. 

### Rerun Benchmark
Have a look on the [Makefile](https://github.com/susanne-207/moc/blob/master/benchmark_rerun/Makefile).

`make train-models` will train the classification models for the benchmark on the tasks derived from OpenML. The id of the tasks are saved in [benchmark_task_ids.rds](https://github.com/susanne-207/moc/blob/master/helpers/benchmark_task_ids.rds).
The models are saved in *saved_objects_rerun*.

`make run-moc` will run the benchmark for MOC. 

`make run-pair` will run the benchmark for Pair.

`make run-tweaking` will run the benchmark for Tweaking. 

Recourse and DiCE have a seperate [Makefile](https://github.com/susanne-207/moc/blob/master/benchmark_rerun/python_recoures_dice/Makefile) since they are Python and not R based.
At first a virtual environment is necessary using `make venv-dice` and `make venv-recourse`. 
To run the experiments, use `make all`. 
