library(tidyverse)
library(mlr3pipelines)
options(rgl.useNULL = TRUE)
library(rgl)
library(Rmpfr)
library(randomForest)
library(data.table)
library(fairml)
library(ggforce)
library(Rtsne)
library(mlr3)
theme_set(theme_bw(18))

set.seed(142)
df_adult = fairml::adult
df_compas = fairml::compas
df_raw_law = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/law_data.csv")
df_lawschool = subset(df_raw_law, select = -X )

plot_tSNE = function(name = "compas"){
  df_compas <- df_compas %>% 
        drop_na()
  df_compas <- df_compas %>% distinct()
  
  x_interest = df_compas[17L, ]
  rf = randomForest(two_year_recid ~ ., data = df_compas[-17L, ])
  predictor = iml::Predictor$new(rf, type = "prob")
  fairness_obj = FairnessTest$new(predictor, df = compas, sensitive_attribute = "race", n_generations = 175)
  diff = fairness_obj$get_difference(x_interest, desired_level = "Caucasian", desired_prob = c(0.5,1))
  
  df_data = as.data.frame(df_compas)
  df_data["type"] = "original data"
  y = predictor$data$y.names
  idx_y = which(data.frame(colnames(x_interest)) == y)
  x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y))
  row_num = as.integer(row.names(match_df(df_data, x_interest_wo_tyr)))
  df_data[row_num, ]$type = "x_interest"
  cf_data = fairness_obj$.__enclos_env__$private$get_counterfactuals()
  cf_data["type"] = "counterfactuals"
  df_data <- subset(df_data, select = -c(idx_y))
  
  df_merged = rbind(cf_data, df_data)
  df_merged <- df_merged %>% distinct()
  
  df_merged <- df_merged %>%
    drop_na() %>%
    dplyr::mutate(ID=row_number())
  
  recidivism_meta <- df_merged %>%
    select(ID, type, race, sex)
  
  y = predictor$data$y.names

  df_merged <- df_merged %>% distinct()
  df_merged <- df_merged %>% drop_na()
  
  df_data = as.data.frame(model.matrix(~., df_merged))
  rownames(df_data) = NULL
  print(df_data)
  
  set.seed(142)
  # browser()
  tSNE_fit <- df_data %>%
    select(where(is.numeric)) %>%
    column_to_rownames("ID") %>%
    as.data.frame() %>%
    # print() %>%
    drop_na() %>%
    Rtsne::Rtsne(check_duplicates = FALSE)
  
  tSNE_df <- tSNE_fit$Y %>%
    as.data.frame() %>%
    dplyr::rename(tSNE1="V1",
                  tSNE2="V2") %>%
    dplyr::mutate(ID=row_number())
  
  tSNE_df <- tSNE_df %>%
    dplyr::inner_join(recidivism_meta, by="ID")
  
  # print(tSNE_df %>% head())
  
  idx = which(tSNE_df$type == "x_interest")
  
  library(ggplot2)
  tSNE_df %>%
    ggplot2::ggplot(aes(x = tSNE1, y = tSNE2,))+ geom_point(aes(shape=sex, color=race, size=type)) + 
    scale_size_manual(values=c(3,1,4)) + geom_circle(aes(x0 = tSNE_df[idx,]$tSNE1, y0 = tSNE_df[idx,]$tSNE2, r = 2),
                                                     color="green", inherit.aes = FALSE) + theme(legend.position="bottom")
  
}
# tSNE_plot_compas = function(){
#   compas <- df_compas %>% 
#     drop_na()
#   compas <- compas %>% distinct()
#   
#   x_interest = compas[17L, ]
#   rf = randomForest(two_year_recid ~ ., data = compas[-17L, ])
#   predictor = iml::Predictor$new(rf, type = "prob")
#   fairness_obj = FairnessTest$new(predictor, df = compas, sensitive_attribute = "race", n_generations = 175)
#   diff = fairness_obj$get_difference(x_interest, desired_level = "Caucasian", desired_prob = c(0.5,1))
#   
#   df_data = as.data.frame(df_compas)
#   df_data["type"] = "original data"
#   df_data[2L, ]$type = "x_interest"
#   
#   cf_data = fairness_obj$get_counterfactuals()
#   cf_data = as.data.frame(cf_data)
#   cf_data["type"] = "counterfactuals"
#   cf_data <- subset(cf_data, select = -c(17, 18))
#   
#   df_merged = rbind(cf_data, df_data)
#   df_merged <- df_merged %>% distinct()
#   
#   df_merged <- df_merged %>% 
#     drop_na() %>%
#     mutate(ID=row_number())
#   
#   recidivism_meta <- df_merged %>%
#     select(ID, type, two_year_recid, race)
#   
#   task = TaskClassif$new("task", df_merged, "two_year_recid")
#   poe = po("encode")
#   df_data = as.data.frame(poe$train(list(task))[[1]]$data())
#   
#   set.seed(142)
#   tSNE_fit <- df_data %>%
#     select(where(is.numeric)) %>%
#     column_to_rownames("ID") %>%
#     scale() %>%
#     Rtsne(check_duplicates = TRUE)
#   
#   
#   tSNE_df <- tSNE_fit$Y %>%
#     as.data.frame() %>%
#     rename(tSNE1="V1",
#            tSNE2="V2") %>%
#     mutate(ID=row_number())
#   
#   tSNE_df <- tSNE_df %>%
#     inner_join(recidivism_meta, by="ID")
#   
#   tSNE_df %>% head()
#   
#   idx = which(tSNE_df$type == "x_interest")
#   idx_c = which(tSNE_df$type == "counterfactuals")
#   
#   library(ggplot2)
#   tSNE_df %>%
#     ggplot2::ggplot(aes(x = tSNE1,
#                         y = tSNE2,))+ geom_point(aes(shape=type, color=race, size=type)) + scale_size_manual(values=c(3,1,4)) + geom_circle(aes(x0 = tSNE_df[idx,]$tSNE1, y0 = tSNE_df[idx,]$tSNE2, r = 2),
#                                                                     color="green",
#                                                                     inherit.aes = FALSE) + theme(legend.position="bottom")
# }

tSNE_plot_adult = function() {
  df_adult <- df_adult %>% 
    drop_na()
  df_adult_distinct <- df_adult %>% distinct()
  
  rf1 = randomForest(income ~ ., data = df_adult_distinct[-79L, ])
  predictor1 = iml::Predictor$new(rf1, type = "prob")
  fairness_obj1 = FairnessTestAdult$new(predictor1, df = df_adult_distinct, column = "sex", row_num = 79L, desired_class = "Male")
  diff1 = fairness_obj1$get_difference()
  
  df_data1 = as.data.frame(df_adult_distinct)
  df_data1["type"] = "original data"
  df_data1[79L, ]$type = "x_interest"
  
  cf_data1 = fairness_obj1$get_dataset_for_tSNE()
  cf_data1 = as.data.frame(cf_data1)
  cf_data1["type"] = "counterfactuals"
  cf_data1 <- subset(cf_data1, select = -c(15, 16))
  df_merged1 = rbind(cf_data1, df_data1)
  df_merged_distinct <- df_merged1 %>% distinct()
  
  df_data1 <- df_merged_distinct %>%
    drop_na() %>%
    mutate(ID=row_number())
  
  adult_meta <- df_data1 %>%
    select(ID, type, income, sex)
  
  task1 = TaskClassif$new("task", df_merged_distinct, "income")
  poe1 = po("encode")
  df_data1 = as.data.frame(poe1$train(list(task1))[[1]]$data())

  # df_data1 <- df_data1 %>%
  #   drop_na() %>%
  #   mutate(ID=row_number())
  # 
  # adult_meta <- df_data1 %>%
  #   select(ID, type, income, sex)
  
  set.seed(142)
  tSNE_fit1 <- df_data1_distinct %>%
    select(where(is.numeric)) %>%
    column_to_rownames("ID") %>%
    Rtsne(check_duplicates = FALSE)

  
  tSNE_df1 <- tSNE_fit1$Y %>%
    as.data.frame() %>%
    rename(tSNE1="V1",
           tSNE2="V2") %>%
    mutate(ID=row_number())
  


  tSNE_df1 <- tSNE_df1 %>%
    inner_join(adult_meta, by="ID")

  idx = which(tSNE_df1$type == "x_interest")
  library(ggplot2)
  
  
  tSNE_df1 %>%
    ggplot2::ggplot(aes(x = tSNE1,
                        y = tSNE2,))+ geom_point(aes(shape=sex, color=type, size=type)) + scale_size_manual(values=c(4,1,5)) + geom_circle(aes(x0 = tSNE_df1[idx,]$tSNE1, y0 = tSNE_df1[idx,]$tSNE2, r = 1),
                                                                                                                                            color="red",
                                                                                                                                            inherit.aes = FALSE) + theme(legend.position="bottom")
}
