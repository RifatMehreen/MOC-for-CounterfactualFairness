library(tidyverse)
library(Rtsne)
library(mlr3)
library(mlr3pipelines)
#library(fairness)
options(rgl.useNULL = TRUE)
library(rgl)
library(Rmpfr)
library(fairml)
library(iml)
library(counterfactuals)
library(randomForest)
library(R6)
library(paradox)
library(miesmuschel)
library(checkmate)
library(data.table)
theme_set(theme_bw(18))

df_compas = fairml::compas


generate_counterfactuals_NICE = function(){
  set.seed(142)
  df_compas <- df_compas %>% drop_na()
  df_compas <- df_compas %>% distinct()
  
  # predictors: one for race and another for two years recidivism
  rf = randomForest(race ~ ., data = df_compas[-1133L, ])
  rf1 = randomForest(two_year_recid ~ ., data = df_compas[-1133L, ])
  
  # generating counterfactuals with NICE classifier
  predictor = iml::Predictor$new(rf, type = "prob")
  predictor1 = iml::Predictor$new(rf1, type = "prob")
  
  nice_classif = NICEClassif$new(predictor)
  cfactuals = nice_classif$find_counterfactuals(x_interest = df_compas[1133L, ], desired_class = "Caucasian", desired_prob = c(0.5, 1))
  
  data_archive = nice_classif$archive
  cf_data = as.data.frame(data_archive[3])
  data_cfactuals <- cf_data %>% filter(cf_data$Caucasian >= 0.5)
  data_cfactuals = as.data.frame(data_cfactuals)
  
  data_cfactuals <- subset(data_cfactuals, select = -c(16:22))
  data_cfactuals["race"] = "Caucasian"
  x_interest = df_compas[1133L, ] 
  data_cfactuals = rbind(x_interest , data_cfactuals)
  data_cfactuals = data_cfactuals[-1,]
  
  
  print("x_interest:")
  print(x_interest)
  print("prediction probability for x_interest:")
  print(predictor1$predict(x_interest))
  print("counterfactuals:")
  print(data_cfactuals)
  print("prediction probability for counterfactuals:")
  print(predictor1$predict(data_cfactuals))
  #cf_data = as.data.frame(cfactuals)
  #return(nice_classif$archive)
  
}
