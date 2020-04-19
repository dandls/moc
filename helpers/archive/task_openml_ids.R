tasks_irace = c(
    14965,  
    37, 
    3917, 
    219, 
    52945, 
    3483, 
    3822, 
    3586) 

tasks_benchmark = c(
    9971,
    167120,
    3,
    3778,
    3913,
    145804,
    3918,
    3718,
    3749, 
    145976, 
    3846)

  

# 9946, # Too many features 
# 3902, # Too many features
# 34537, # Too Big! 
# 3903, # too many features
# 3778, # too many features
#     #167120, # numerai28.6, 
#  #  14965, # bank-marketing
#  #  167141, # churn
#  #  146820, # wilt
#  # # 146819, # climate-model-simulation-crashes --> Study und RUN: correlations!! 
#  #  9946, # wdbc
#  #  9971, # ilpd
#  #  9952, # phoneme
#  #  9957, # qsar-biodeg
#  #  3, # kr-vs-kp
#  #  3903, # pc3
#  #  3902, # pc4
#  #  37, # diabetes
#  #  3917, # kc1
#  #  3913, # kc2
#  #  219, # electricity
#  #  145804, # tic-tac-toe
#  #  3918, # pc-1
#  #  34537, # PhishingWebsites
#  #  10093, # banknote-authentication
#  #  31, # credit-g
#  #  10101, # blood-transfusion-service-center
#   3718, # boston
#   52945, # breastTumor
#   3778, # plasma_retinol
#   145979, # spambase
#   3818, # tae
#   # 146230, # titanic
#   3483, # mammography
#   3822, # nursery
#   3586,
#   3877, # analcatdata  Too small
#   3846, # haberman
#   145976, #iris
#   42, # no2 
#   3832, # diabetes
#   3749, # cmc
#   167141 # churn 
#   # 3872 # white-clover 
# )

# mlr_tasks = lapply(tasks_benchmark, function(task.id) {
#   t = getOMLTask(task.id = task.id)
#   print(str(t$input$data.set$data))
#   t$input$data.set$data
# })
