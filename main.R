setwd("D:/ProgramData/Jupyter Notebook/UG4")
library(dplyr)
library(tmle3)
library(sl3)
library(glue)

data_folder <- "D:/ProgramData/Jupyter Notebook/UG4/results/NHS"
result_folder <- "D:/ProgramData/Jupyter Notebook/UG4/results/NHS/TMLE"
df <- read.csv(glue("{data_folder}/df_agg.csv"))

node_list <- list(
  W = c("AL_mean", "sleep_percentage"),
  A = "treatment",
  Y = "score"
)

ate_spec <- tmle_ATE(
  treatment_level = 1,
  control_level = 0
)

# processed <- process_missing(df, node_list)
# df <- processed$data
# node_list <- processed$node_list
# df <- df %>% mutate_if(is.numeric, round, digits=2)

# # choose base learners
# lrnr_mean <- make_learner(Lrnr_mean)
# lrnr_rf <- make_learner(Lrnr_ranger)
# 
# # define metalearners appropriate to data types
# ls_metalearner <- make_learner(Lrnr_nnls)
# mn_metalearner <- make_learner(
#   Lrnr_solnp, metalearner_linear_multinomial,
#   loss_loglik_multinomial
# )
# sl_Y <- Lrnr_sl$new(
#   learners = list(lrnr_mean, lrnr_rf),
#   metalearner = ls_metalearner
# )
# sl_A <- Lrnr_sl$new(
#   learners = list(lrnr_mean, lrnr_rf),
#   metalearner = mn_metalearner
# )
# learner_list <- list(A = sl_A, Y = sl_Y)

# n <- 20

# sl3_list_learners(properties = "continuous")

get_learner_list <- function(n){
  # choose base learners
  lrn_glm <- Lrnr_glm$new()
  # lrn_glmnet <- Lrnr_glmnet$new()
  lrn_xgboost <- Lrnr_xgboost$new()
  lrn_ranger <- Lrnr_ranger$new()
  # lrn_svm <- Lrnr_svm$new()
  # lrn_caret <- Lrnr_caret$new(method = "nnet", name = "NNET_autotune")
  
  base_learners <- Stack$new(
    lrn_glm, 
    # lrn_glmnet, 
    lrn_xgboost, 
    lrn_ranger
    # lrn_svm, 
    # lrn_caret
  )
  
  # define metalearners appropriate to data types
  treatment_metalearner <- make_learner(Lrnr_nnls)
  
  outcome_metalearner <- make_learner(
    # Lrnr_glm,
    Lrnr_solnp,
    loss_function = loss_loglik_true_cat,
    learner_function = metalearner_linear,
    eval_function = loss_loglik_true_cat
  )
  
  # treatment_metalearner <- make_learner(
  #   Lrnr_solnp, 
  #   loss_function = loss_loglik_binomial,
  #   learner_function = metalearner_logistic_binomial, 
  #   eval_function = loss_squared_error
  # )
  # 
  # cv_selector <- Lrnr_cv_selector$new(eval_function = loss_squared_error)
  # dSL <- Lrnr_sl$new(learners = stack, metalearner = cv_selector)
  # 
  # outcome_metalearner <- make_learner(
  #   Lrnr_solnp,
  #   # If continous loss_loglik_true_cat and metalearner_linear
  #   loss_function = loss_squared_error,
  #   learner_function =  metalearner_logistic_binomial,
  #   eval_function = loss_squared_error
  # )
  
  sl_Y <- Lrnr_sl$new(
    learners = base_learners,
    metalearner = treatment_metalearner, 
    cv_folds = n
  )
  sl_A <- Lrnr_sl$new(
    learners = base_learners,
    metalearner = outcome_metalearner, 
    cv_folds = n
  )
  
  learner_list <- list(A = sl_A, Y = sl_Y)
  return(learner_list)
}

# get_learner_list()

# tmle_fit <- tmle3(ate_spec, df, node_list, get_learner_list())
# print(tmle_fit)
# 
# estimates <- tmle_fit$summary$psi_transformed
# print(estimates)

# columns <- c('BR_mean', 'tidal_area', 'breath_duration', 'peak_respiratory_flow', 
#              'BR_mean_sleep', 'tidal_area_sleep', 'breath_duration_sleep',
#              'peak_respiratory_flow_sleep', 
#              'BR_mean_awake', 'tidal_area_awake', 'breath_duration_awake',
#              'peak_respiratory_flow_awake')

columns <- c('BR_mean', 'tidal_area', 'breath_duration', 'peak_respiratory_flow')
ps <- c(0.65,  0.75, 0.85)

for (p in ps) {
  
  tmle_fits <- matrix(nrow=0, ncol=3)
  print(p)
  
  for (t in columns) {
  
    print(glue("Starting TMLE for {t}:"))
  
    tryCatch(
      for (id in c(1)) {
        df_id <- df
        n <- 20
  
        quantile_p <- quantile(df_id[[t]], p)
        df_id$treatment <- ifelse(df_id[[t]] > quantile_p, 1, 0)
  
        processed <- process_missing(df_id, node_list)
        df_id <- processed$data
        node_list <- processed$node_list
        df_id <- df_id %>% mutate_if(is.numeric, round, digits=2)
        # View(df_id)
  
        tmle_fit <- tmle3(ate_spec, df_id, node_list, get_learner_list(n))
  
        result <- c(tmle_fit$summary$tmle_est, tmle_fit$summary$se)
        print(result)
  
        tmle_fits <- rbind(tmle_fits, c(t, result))
      },
      error=function(e) {
        message(glue('An Error Occurred in {id}'))
        print(e)
      }
    )
  }
  # output_file <- glue("{result_folder}/with_CAT/tmle_quantile{p}.csv")
  output_file <- glue("{result_folder}/agg/tmle_quantile{p}.csv")
  write.csv(tmle_fits, output_file, row.names=FALSE)
}


ids <- c('PRB001', 'PRB003', 'PRB005', 'PRB006', 'PRB007', 'PRB102',
         'PRB103', 'PRB104', 'PRB105', 'PRB107', 'PRB108', 'PRB109',
         'PRB201', 'PRB202', 'PRB203', 'PRX018', 'PRX900')
ps <- c(0.65,  0.75, 0.85)

for (p in ps) {
  
  print(p)
  
  for (t in columns) {
    
    print(glue("Starting TMLE for {t}:"))
    tmle_fits <- matrix(nrow=0, ncol=3)
    
    tryCatch(
      {
      for (id in ids) {
        
        print(glue("For {id}..."))
        
        df_id <- df[df$id == id, ]
        n <- 20
        if (nrow(df_id) < 30){
          n <- nrow(df_id)
        }
        if (n < 10) {
          print("Not enough data!")
          next
        }
  
        quantile_p <- quantile(df_id[[t]], p)
        df_id$treatment <- ifelse(df_id[[t]] > quantile_p, 1, 0)
  
        processed <- process_missing(df_id, node_list)
        df_id <- processed$data
        node_list <- processed$node_list
        df_id <- df_id %>% mutate_if(is.numeric, round, digits=2)
  
        tmle_fit <- tmle3(ate_spec, df_id, node_list, get_learner_list(n))
  
        result <- c(tmle_fit$summary$tmle_est, tmle_fit$summary$se)
        print(result)
  
        tmle_fits <- rbind(tmle_fits, c(id, result))
      }
      output_file <- glue("{result_folder}/tmle_{t}_quantile{p}.csv")
      write.csv(tmle_fits, output_file, row.names=FALSE)
      },
      error=function(e) {
        message(glue('An Error Occurred in {id}'))
        print(e)
      }
    )
  }
}

