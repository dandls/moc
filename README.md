# MOC - Multi-Objective Counterfactuals

This repository provides code and examples for generating multi-objective counterfactuals for the following paper:      
Dandl, S., Molnar, C., Binder, M., Bischl, B. (2020): Multi-Objective Counterfactual Expalantions. 

For all computations, we used either the statistical software R (version ≥ 3.4.4) or Python (version 3.6.9)

## Overview 

* **Code to reproduce analysis done in the Paper**:
    * *credit_example*: Example R code that generates counterfactuals on the German credit dataset, as used in the Paper. 
    * *saved_objects*: Saved best parameters tuned by irace. 
* **Package Code**:
    * *counterfactuals*: [`counterfactuals`](counterfactuals/) package in an early state. To be released as an R package in the future.

## Manual 

### Download the github repository 

```
git clone https://github.com/susanne-207/moc.git
``` 

### Statistical Analysis
For the German Credit dataset example shown in the paper, step through this file:
[german_credit_application.R](credit_example/german_credit_application.R) 

