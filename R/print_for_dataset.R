print_for_dataset_MOC_rf = function(main_data, df, y, sen_attribute, desired_level, n_generation, desired_prob){
  final_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(final_df) <- c('instance', 'prediction_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)')
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    #print(x_interest)
    
    est = as.formula(paste(substitute(y), " ~ ."))
    set.seed(142)
    rf = randomForest(est, data = main_data[-(row_num), ])
    predictor = iml::Predictor$new(rf, type = "prob", data = main_data[-(row_num), ])
    pred_x = as.data.frame(predictor$predict(x_interest))
    
    n = predictor$data$y.names
    c = names(pred_x)
    name_col = c[c==x_interest[[n]]]
    
    fairness_obj = FairnessTest$new(predictor, df = main_data, sensitive_attribute = sen_attribute, n_generations = n_generation)
    difference = fairness_obj$get_prediction_difference(x_interest, desired_level = desired_level, desired_prob = desired_prob)
    endTime <- Sys.time()
    percentages = fairness_obj$prediction_percentages(x_interest)
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = pred_x[, name_col]
    final_df[[3]][i] = fairness_obj$get_prediction_difference_mean()
    final_df[[4]][i] = fairness_obj$get_cfactuals_count()
    final_df[[5]][i] = round((endTime - startTime),2)
    
    percentage = fairness_obj$prediction_percentages(x_interest)
    if(i==1){
      name1 = names(percentage)[1]
      name2 = names(percentage)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }
    
    final_df[[6]][i] = percentage[[1]]
    final_df[[7]][i] = percentage[[2]]
    
  }
  # return(cbind(df, final_df))
  return(final_df)
  
}

print_for_dataset_MOC_log_reg = function(main_data, df, y, sen_attribute, desired_level, n_generation, desired_prob){
  final_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(final_df) <- c('instance', 'prediction_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)')
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    # learner for the logistic regression
    learner = mlr_learners$get("classif.log_reg")
    task = as_task_classif(main_data, target = y)
    learner$predict_type = "prob"
    
    train_set = sample(task$nrow, 0.8 * task$nrow)
    test_set = setdiff(seq_len(task$nrow), train_set)
    if(any(train_set == row_num)){
      train_set = train_set[train_set != row_num]
      append(test_set, row_num)
    }
    
    set.seed(142)
    learner$train(task, row_ids = train_set)
    predictor = iml::Predictor$new(learner, data = task$data(), y = y)
   
    fairness_obj = FairnessTest$new(predictor, df = main_data, sensitive_attribute = sen_attribute, n_generations = n_generation)
    difference = try(fairness_obj$get_prediction_difference(x_interest, desired_level = desired_level, desired_prob = desired_prob))
    endTime <- Sys.time()
    percentages = fairness_obj$prediction_percentages(x_interest)
    
    pred_x = predictor$predict(x_interest)
    
    n = predictor$data$y.names
    c = names(pred_x)
    name_col = c[c==x_interest[[n]]]
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = pred_x[, name_col]
    final_df[[3]][i] = fairness_obj$get_prediction_difference_mean()
    final_df[[4]][i] = fairness_obj$get_cfactuals_count()
    final_df[[5]][i] = round((endTime - startTime),2)
    
    percentage = fairness_obj$prediction_percentages(x_interest)
    if(i==1){
      name1 = names(percentage)[1]
      name2 = names(percentage)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }
    
    final_df[[6]][i] = percentage[[1]]
    final_df[[7]][i] = percentage[[2]]
  }
  # return(cbind(df, final_df))
  return(final_df)
  
}

print_for_dataset_NICE_rf = function(main_data, df, y, sen_attribute, desired_level, n_generation, desired_prob){
  final_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(final_df) <- c('instance', 'prediction_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)')
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    # main predictor
    est = as.formula(paste(substitute(y), " ~ ."))
    set.seed(142)
    rf = randomForest(est, data = main_data[-(row_num), ])
    predictor = iml::Predictor$new(rf, type = "prob", data = main_data[-(row_num), ])
    
    # generating counterfactuals with NICE classifier
    est_protected = as.formula(paste(substitute(sen_attribute), " ~ ."))
    set.seed(142)
    rf_protected = randomForest(est_protected, data = main_data[-(row_num), ])
    predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = main_data[-(row_num), ])
    nice_classif = NICEClassif$new(predictor_protected)
    cfactuals = nice_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
    
    endTime <- Sys.time()
    
    pred_x = predictor$predict(x_interest)
    
    n = predictor$data$y.names
    c = names(pred_x)
    name_col = c[c==x_interest[[n]]]
    
    data_archive = nice_classif$archive
    len = length(data_archive)
    cf_data = as.data.frame(data_archive[len])
    data_cfactuals <- cf_data %>% filter(cf_data[desired_level] >= 0.5)
    data_cfactuals = as.data.frame(data_cfactuals)
    
    idx_y_x = which(data.frame(colnames(x_interest)) == y)
    x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
    idx_y = which(data.frame(colnames(data_cfactuals)) == y)
    data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))

    
    data_cfactuals <- subset(data_cfactuals, select = -c(grep("reward", colnames(data_cfactuals)):length(data_cfactuals)))
    data_cfactuals[sen_attribute] = desired_level
    #print(x_interest_wo_tyr)
    #print(data_cfactuals)
    data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
    data_cfactuals = data_cfactuals[-1,]
    
    # print("prediction probability for x_interest:")
    pred_x_interest = predictor$predict(x_interest)
    pred_cfactuals = predictor$predict(data_cfactuals)
    
    df_merged = cbind(data_cfactuals, pred_cfactuals)
    
    c = names(pred_x_interest)
    name_col = c[c!=x_interest[[y]]]
    
    df_merged$diff_from_instance = abs((df_merged[, ..name_col]) - (pred_x_interest[, name_col]))
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = pred_x[, name_col]
    final_df[[3]][i] = mean(df_merged$diff_from_instance)

    final_df[[4]][i] = nrow(data_cfactuals)
    final_df[[5]][i] = round((endTime - startTime),2)
    
    level = names(pred_x_interest)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))
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
    
    
    if(i==1){
      name1 = names(pred_cfactuals)[1]
      name2 = names(pred_cfactuals)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }
    final_df[[6]][i] = d[1]
    final_df[[7]][i] = d[2]
    
  }
  
  return(final_df)
  
}

print_for_dataset_NICE_log_reg = function(main_data, df, y, sen_attribute, desired_level, n_generation, desired_prob){
  final_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(final_df) <- c('instance', 'prediction_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)')
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    # learner for the logistic regression
    learner = mlr_learners$get("classif.log_reg")
    task = as_task_classif(main_data, target = y)
    learner$predict_type = "prob"
    
    train_set = sample(task$nrow, 0.8 * task$nrow)
    test_set = setdiff(seq_len(task$nrow), train_set)
    if(any(train_set == row_num)){
      train_set = train_set[train_set != row_num]
      append(test_set, row_num)
    }
    
    set.seed(142)
    learner$train(task, row_ids = train_set)
    predictor = iml::Predictor$new(learner, data = task$data(), y = y)
    
    # generating counterfactuals with NICE classifier
    est_protected = as.formula(paste(substitute(sen_attribute), " ~ ."))
    set.seed(142)
    rf_protected = randomForest(est_protected, data = main_data[-(row_num), ])
    predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = main_data[-(row_num), ])
    nice_classif = NICEClassif$new(predictor_protected)
    cfactuals = nice_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
    
    endTime <- Sys.time()
    
    pred_x = predictor$predict(x_interest)
    
    n = predictor$data$y.names
    c = names(pred_x)
    name_col = c[c==x_interest[[n]]]
    
    data_archive = nice_classif$archive
    len = length(data_archive)
    cf_data = as.data.frame(data_archive[len])
    data_cfactuals <- cf_data %>% filter(cf_data[desired_level] >= 0.5)
    data_cfactuals = as.data.frame(data_cfactuals)
    
    idx_y_x = which(data.frame(colnames(x_interest)) == y)
    x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
    idx_y = which(data.frame(colnames(data_cfactuals)) == y)
    data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))
    
    data_cfactuals <- subset(data_cfactuals, select = -c(grep("reward", colnames(data_cfactuals)):length(data_cfactuals)))
    data_cfactuals[sen_attribute] = desired_level
    data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
    data_cfactuals = data_cfactuals[-1,]
    
    pred_x_interest = predictor$predict(x_interest)
    pred_cfactuals = predictor$predict(data_cfactuals)
    
    df_merged = cbind(data_cfactuals, pred_cfactuals)
    
    c = names(pred_x_interest)
    name_col = c[c!=x_interest[[y]]]
    
    df_merged$diff_from_instance = abs((df_merged[, ..name_col]) - (pred_x_interest[, name_col]))
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = pred_x[, name_col]
    final_df[[3]][i] = mean(df_merged$diff_from_instance)
    final_df[[4]][i] = nrow(data_cfactuals)
    final_df[[5]][i] = round((endTime - startTime),2)
    
    level = names(pred_x_interest)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))

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
    
    
    if(i==1){
      name1 = names(pred_cfactuals)[1]
      name2 = names(pred_cfactuals)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }

    final_df[[6]][i] = d[1]
    final_df[[7]][i] = d[2]
    
  }
  
  return(final_df)
  
}

print_for_dataset_whatif_rf = function(main_data, df, y, sen_attribute, desired_level, desired_prob){
  final_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(final_df) <- c('instance', 'prediction_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)')
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    # main predictor
    est = as.formula(paste(substitute(y), " ~ ."))
    set.seed(142)
    rf = randomForest(est, data = main_data[-(row_num), ])
    predictor = iml::Predictor$new(rf, type = "prob", data = main_data[-(row_num), ])
    
    # generating counterfactuals with Whatif classifier
    est_protected = as.formula(paste(substitute(sen_attribute), " ~ ."))
    set.seed(142)
    rf_protected = randomForest(est_protected, data = main_data[-(row_num), ])
    predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = main_data[-(row_num), ])
    whatif_classif = WhatIfClassif$new(predictor_protected)
    cfactuals = whatif_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
    
    endTime <- Sys.time()
    
    pred_x = predictor$predict(x_interest)
    
    n = predictor$data$y.names
    c = names(pred_x)
    name_col = c[c==x_interest[[n]]]
    
    data_cfactuals = as.data.frame(cfactuals$data)
    
    idx_y_x = which(data.frame(colnames(x_interest)) == y)
    x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
    idx_y = which(data.frame(colnames(data_cfactuals)) == y)
    data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))
    
    data_cfactuals[sen_attribute] = desired_level
    data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
    data_cfactuals = data_cfactuals[-1,]
    
    # print("prediction probability for x_interest:")
    pred_x_interest = predictor$predict(x_interest)
    pred_cfactuals = predictor$predict(data_cfactuals)
    
    df_merged = cbind(data_cfactuals, pred_cfactuals)
    
    c = names(pred_x_interest)
    name_col = c[c!=x_interest[[y]]]
    
    df_merged$diff_from_instance = abs((df_merged[, ..name_col]) - (pred_x_interest[, name_col]))
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = pred_x[, name_col]
    final_df[[3]][i] = mean(df_merged$diff_from_instance)
    final_df[[4]][i] = nrow(data_cfactuals)
    final_df[[5]][i] = round((endTime - startTime),2)
    
    level = names(pred_x_interest)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))
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
    
    
    if(i==1){
      name1 = names(pred_cfactuals)[1]
      name2 = names(pred_cfactuals)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }
    final_df[[6]][i] = d[1]
    final_df[[7]][i] = d[2]
    
  }
  
  return(final_df)
  
}


print_for_dataset_whatif_rf = function(main_data, df, y, sen_attribute, desired_level, desired_prob){
  final_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(final_df) <- c('instance', 'prediction_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)')
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    # main predictor
    est = as.formula(paste(substitute(y), " ~ ."))
    set.seed(142)
    rf = randomForest(est, data = main_data[-(row_num), ])
    predictor = iml::Predictor$new(rf, type = "prob", data = main_data[-(row_num), ])
    
    # generating counterfactuals with Whatif classifier
    est_protected = as.formula(paste(substitute(sen_attribute), " ~ ."))
    set.seed(142)
    rf_protected = randomForest(est_protected, data = main_data[-(row_num), ])
    predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = main_data[-(row_num), ])
    whatif_classif = WhatIfClassif$new(predictor_protected)
    cfactuals = whatif_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
    
    endTime <- Sys.time()
    
    pred_x = predictor$predict(x_interest)
    
    n = predictor$data$y.names
    c = names(pred_x)
    name_col = c[c==x_interest[[n]]]
    
    data_cfactuals = as.data.frame(cfactuals$data)
    
    idx_y_x = which(data.frame(colnames(x_interest)) == y)
    x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
    idx_y = which(data.frame(colnames(data_cfactuals)) == y)
    data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))
    
    data_cfactuals[sen_attribute] = desired_level
    data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
    data_cfactuals = data_cfactuals[-1,]
    
    # print("prediction probability for x_interest:")
    pred_x_interest = predictor$predict(x_interest)
    pred_cfactuals = predictor$predict(data_cfactuals)
    
    df_merged = cbind(data_cfactuals, pred_cfactuals)
    
    c = names(pred_x_interest)
    name_col = c[c!=x_interest[[y]]]
    
    df_merged$diff_from_instance = abs((df_merged[, ..name_col]) - (pred_x_interest[, name_col]))
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = pred_x[, name_col]
    final_df[[3]][i] = mean(df_merged$diff_from_instance)
    final_df[[4]][i] = nrow(data_cfactuals)
    final_df[[5]][i] = round((endTime - startTime),2)
    
    level = names(pred_x_interest)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))
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
    
    
    if(i==1){
      name1 = names(pred_cfactuals)[1]
      name2 = names(pred_cfactuals)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }
    final_df[[6]][i] = d[1]
    final_df[[7]][i] = d[2]
    
  }
  
  return(final_df)
  
}

print_for_dataset_whatif_log_reg = function(main_data, df, y, sen_attribute, desired_level, desired_prob){
  final_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(final_df) <- c('instance', 'prediction_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)')
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    # learner for the logistic regression
    learner = mlr_learners$get("classif.log_reg")
    task = as_task_classif(main_data, target = y)
    learner$predict_type = "prob"
    
    train_set = sample(task$nrow, 0.8 * task$nrow)
    test_set = setdiff(seq_len(task$nrow), train_set)
    if(any(train_set == row_num)){
      train_set = train_set[train_set != row_num]
      append(test_set, row_num)
    }
    
    set.seed(142)
    learner$train(task, row_ids = train_set)
    predictor = iml::Predictor$new(learner, data = task$data(), y = y)
    
    # generating counterfactuals with Whatif classifier
    est_protected = as.formula(paste(substitute(sen_attribute), " ~ ."))
    set.seed(142)
    rf_protected = randomForest(est_protected, data = main_data[-(row_num), ])
    predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = main_data[-(row_num), ])
    whatif_classif = WhatIfClassif$new(predictor_protected)
    cfactuals = whatif_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
    
    endTime <- Sys.time()
    
    pred_x = predictor$predict(x_interest)
    
    n = predictor$data$y.names
    c = names(pred_x)
    name_col = c[c==x_interest[[n]]]
    
    data_cfactuals = as.data.frame(cfactuals$data)
    
    idx_y_x = which(data.frame(colnames(x_interest)) == y)
    x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
    idx_y = which(data.frame(colnames(data_cfactuals)) == y)
    data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))
    
    data_cfactuals[sen_attribute] = desired_level
    data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
    data_cfactuals = data_cfactuals[-1,]
    
    # print("prediction probability for x_interest:")
    pred_x_interest = predictor$predict(x_interest)
    pred_cfactuals = predictor$predict(data_cfactuals)
    
    df_merged = cbind(data_cfactuals, pred_cfactuals)
    
    c = names(pred_x_interest)
    name_col = c[c!=x_interest[[y]]]
    
    df_merged$diff_from_instance = abs((df_merged[, ..name_col]) - (pred_x_interest[, name_col]))
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = pred_x[, name_col]
    final_df[[3]][i] = mean(df_merged$diff_from_instance)
    final_df[[4]][i] = nrow(data_cfactuals)
    final_df[[5]][i] = round((endTime - startTime),2)
    
    level = names(pred_x_interest)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))
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
    
    
    if(i==1){
      name1 = names(pred_cfactuals)[1]
      name2 = names(pred_cfactuals)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }
    final_df[[6]][i] = d[1]
    final_df[[7]][i] = d[2]
    
  }
  
  return(final_df)
  
}

