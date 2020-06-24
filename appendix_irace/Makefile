data_dir = ../saved_objects_rerun/irace
model_dir = ../saved_objects_rerun/irace/models_irace.rds
irace_result_dir = ../saved_objects_rerun/irace/irace_results.rds
config_dir = ../saved_objects_rerun/best_configs.rds
nr_evals_dir = ../saved_objects_rerun/irace/max_eval.rds

all : train-models get-evals run-irace tune-evals

train-models : 
	R CMD BATCH '--args FALSE task_ids.rds $(model_dir) $(data_dir) 20 FALSE 1234' ../helpers/train_models.R

get-evals : 
	R CMD BATCH '--args  $(model_dir) $(data_dir) $(nr_evals_dir)' get_evals.R
	
run-irace : 
	R CMD BATCH '--args $(model_dir) $(irace_result_dir) $(nr_evals_dir) $(data_dir)' run_irace.R

get-generations : 
	R CMD BATCH '--args  $(model_dir) $(data_dir) $(irace_result_dir) $(config_dir)' get_generations.R
