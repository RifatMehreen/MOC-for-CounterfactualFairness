library(tidyverse)
library(Rtsne)
library(mlr3)
library(mlr3pipelines)
#library(fairness)
options(rgl.useNULL = TRUE)
library(rgl)
library(Rmpfr)
library(fairml)
library(randomForest)
library(checkmate)
library(data.table)
theme_set(theme_bw(18))

generate_counterfactuals_NICE = function(df, predictor, x_interest, sensitive_att, desired_level, desired_prob){
  row_num = as.integer(row.names(match_df(df, x_interest)))
  
  # generating counterfactuals with NICE classifier
  est = as.formula(paste(substitute(sensitive_att), " ~ ."))
  set.seed(142)
  rf_protected = randomForest(est, data = df[-(row_num), ])
  predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = df[-(row_num), ])
  
  nice_classif = NICEClassif$new(predictor_protected)
  cfactuals = nice_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
  
  data_archive = nice_classif$archive
  len = length(data_archive)
  cf_data = as.data.frame(data_archive[len])
  print("number of generated counterfactuals")
  print(nrow(cf_data))
  print(cf_data)
  data_cfactuals <- cf_data %>% filter(cf_data[desired_level] >= 0.5)
  data_cfactuals = as.data.frame(data_cfactuals)
  
  y = predictor$data$y.names
  print(y)
  idx_y_x = which(data.frame(colnames(x_interest)) == y)
  x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
  print(x_interest_wo_tyr)
  idx_y = which(data.frame(colnames(data_cfactuals)) == y)
  print(idx_y)
  data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))
  
  print("ei je rewaerd ase naki nai bujhtesina baa")
  print(data_cfactuals)
  
  data_cfactuals <- subset(data_cfactuals, select = -c(grep("reward", colnames(data_cfactuals)):length(data_cfactuals)))
  data_cfactuals[sensitive_att] = desired_level
  print(x_interest_wo_tyr)
  print(data_cfactuals)
  data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
  data_cfactuals = data_cfactuals[-1,]
  
  
  print("x_interest:")
  print(x_interest)
  print("prediction probability for x_interest:")
  pred_x_interest = predictor$predict(x_interest)
  print(pred_x_interest)
  print("counterfactuals:")
  print(data_cfactuals)
  print("prediction probability for counterfactuals:")
  print(predictor$predict(data_cfactuals))
  
  # prediction percentages for NICE
  level = names(pred_x_interest)
  print(level)
  new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
  idx_s = which(colnames(new_data) == sensitive_att)
  new_data <- subset(new_data, select = -c(idx_s))
  #return(new_data)
  new_data[[level[1]]] <- ifelse(new_data[[level[1]]] > 0.5, 1, 0)
  new_data[[level[2]]] <- ifelse(new_data[[level[2]]] > 0.5, 1, 0)
  
  percent_cf <- vector(mode = "list", length = 0)
  percent_cf$l1 = round(100 * (length(which(new_data[[level[1]]] == 1))/nrow(new_data)), 2)
  percent_cf$l2 = round(100 * (length(which(new_data[[level[2]]] == 1))/nrow(new_data)), 2)
  
  d = data.frame(name1 = numeric(0), name2 = numeric(0))
  newrow = data.frame(name1 = percent_cf$l1, name2 = percent_cf$l2)
  d <- rbind(d, newrow)
  colnames(d)[1] <- level[1]
  colnames(d)[2] <- level[2]
  
  print(d)

}
