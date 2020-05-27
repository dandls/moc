data_dir = ../saved_objects_rerun/irace
model_dir = ../saved_objects_rerun/irace/models_irace_keras.rds
irace_result_dir = ../saved_objects_rerun/irace/irace_results.rds
config_dir = ../saved_objects/best_configs.rds
nr_evals_dir = ../saved_objects_rerun/irace/max_eval.rds

all : train-models get-evals run-irace tune-evals

train-models : 
	R CMD BATCH '--args TRUE task_ids.rds $(model_dir) $(data_dir)' ../helpers/train_models.R  &
	# Rscript ../helpers/train_models.R --savekeras TRUE --task_ids task_ids.rds --savedir $(model_dir) --data_dir $(data_dir)

get-evals : 
	Rscript get_evals.R --readdir $(model_dir) --savedir $(nr_evals_dir)
	
run-irace : 
	R CMD BATCH '--args $(model_dir) $(irace_result_dir) $(nr_evals_dir) $(data_dir)' run_irace.R &
	# Rscript run_irace.R --readdir $(model_dir) --savedir $(irace_result_dir) --evaldir $(nr_evals_dir) --data_dir $(data_dir)

get-generations : 
	Rscript get_generations.R --readdir $(model_dir) --savedir $(config_dir) --iracedir $(irace_result_dir) --gendir "../saved_objects/max_generations.rds"

