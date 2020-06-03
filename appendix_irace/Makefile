data_dir = ../saved_objects_rerun/irace
model_dir = ../saved_objects_rerun/irace/models_irace.rds
irace_result_dir = ../saved_objects_rerun/irace/irace_results.rds
config_dir = ../saved_objects_rerun/best_configs.rds
nr_evals_dir = ../saved_objects_rerun/irace/max_eval.rds

all : train-models get-evals run-irace tune-evals

train-models : 
	R CMD BATCH '--args FALSE task_ids.rds $(model_dir) $(data_dir) 20 FALSE' ../helpers/train_models.R
	# Rscript ../helpers/train_models.R --savekeras TRUE --task_ids task_ids.rds --savedir $(model_dir) --data_dir $(data_dir)

get-evals : 
	R CMD BATCH '--args  $(model_dir) $(data_dir) $(nr_evals_dir)' get_evals.R
	
run-irace : 
	R CMD BATCH '--args $(model_dir) $(irace_result_dir) $(nr_evals_dir) $(data_dir)' run_irace.R
	# Rscript run_irace.R --readdir $(model_dir) --savedir $(irace_result_dir) --evaldir $(nr_evals_dir) --data_dir $(data_dir)

get-generations : 
	R CMD BATCH '--args  $(model_dir) $(config_dir) $(irace_result_dir) $(nr_evals_dir)' get_generations.R
	# Rscript get_generations.R --readdir $(model_dir) --savedir $(config_dir) --iracedir $(irace_result_dir) --gendir "../saved_objects/max_generations.rds"

