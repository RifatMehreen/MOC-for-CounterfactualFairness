get_predictor <- function(df, column, row_num, desired_class){
  est <- as.formula(paste(substitute(column), " ~ ."))
  rf1 = randomForest(est, data = df[-(row_num), ])
  
  predictor = Predictor$new(rf1, type = "prob", data = df[-(row_num)])
  # moc_classif = MOCClassif$new(predictor, n_generations = 5L)
  # x_interest = compas[row_num, ]
  # cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = desired_class, desired_prob = c(0.5, 1))
  # return(cfactuals)
}



# moc_classif = MOCCounterfactualFairnessClassif$new(predictor, df, protected_column, row_num, predictor_protected, n_generations = 5L)
# x_interest = df[row_num, ]
# # predictor$predict(x_interest)
# cfactuals_protected = moc_classif$find_counterfactuals(x_interest, desired_class = desired_class, desired_prob = c(0.5, 1))

