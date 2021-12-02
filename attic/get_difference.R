#' Generates the differences between the prediction of the original instance and the prediction of the counterfactuals.
#' 
#' @description 
#' Creates a dataframe with the differences of predictions
#' 
#' @param predictor (`iml predictor`)\cr
#' The original predictor where as response variable we use `Two_yr_Recidivism`.
#' @param df (dataframe)\cr
#' The dataset or dataframe for the prediction
#' @param column (string)\cr
#' The name of protected column.
#' @param row_num (numeric())\cr
#' The row number of instance of interest `x_interest`.
#' @param desired_class (string)\cr
#' The desired class we want to have in our counterfactuals
#' 
#' @return A dataframe with the differences of predictions of the original instance and the counterfactuals
#' 

get_difference <- function(predictor, df, column, row_num, desired_class){
  est <- as.formula(paste(substitute(column), " ~ ."))
  rf1 = randomForest(est, data = df[-(row_num), ])
  
  predictor_protected = Predictor$new(rf1, type = "prob", data = df[-(row_num)])
  moc_classif = MOCClassif$new(predictor_protected, n_generations = 175L)
  x_interest = compas[row_num, ]
  cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = desired_class, desired_prob = c(0.5, 1))
  df = as.data.frame(cfactuals$data)
  df[column] = desired_class
  df <- rbind(x_interest , df)
  df <- df[-1,]
  pred_x_interest = predictor$predict(x_interest)
  print(pred_x_interest)
  pred_cfactuals_protected = predictor$predict(df)
  df_pred_prot = as.data.frame(pred_cfactuals_protected)
  df_merged = cbind(df, df_pred_prot)
  df_merged$diff_from_instance = ((df_merged[, 8]) - (pred_x_interest[, 1]))
  # print("Number of counterfactuals")
  # print(nrow(df_merged))
  df_merged = subset(df_merged, no >= 0.5)
  # print("Number of counterfactuals whose prediction as 'no' has probability > 0.5")
  # print(nrow(df_merged))
  df_merged <- df_merged[order(-no),]
}

