get_difference <- function(predictor, df, column, row_num, desired_class){
  est <- as.formula(paste(substitute(column), " ~ ."))
  rf1 = randomForest(est, data = df[-(row_num), ])
  
  predictor_protected = Predictor$new(rf1, type = "prob", data = df[-(row_num)])
  moc_classif = MOCClassif$new(predictor_protected, n_generations = 5L)
  x_interest = compas[row_num, ]
  cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = desired_class, desired_prob = c(0.5, 1))
  df = as.data.frame(cfactuals$data)
  df[column] = desired_class
  df <- rbind(x_interest , df)
  df <- df[-1,]
  #return(cfactuals)
  pred_x_interest = predictor$predict(x_interest)
  #print(df)
  pred_cfactuals_protected = predictor$predict(df)
}

