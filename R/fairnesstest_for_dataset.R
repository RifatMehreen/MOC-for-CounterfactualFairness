#' fairnesstest_moc
#' 
#' A function to print the MBE and instance(s) with ampd for moc
#'
#' @return A character vector
#' @export

fairnesstest_moc = function(main_data, df, y, sen_attribute, desired_level, fixed_features = NULL, n_generation, desired_prob, model){
  final_df <- data.frame(matrix(ncol = 7, nrow = nrow(df)))
  colnames(final_df) <- c('instance_id', 'pred_x_interest', 'pred_prob_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)', 'ampd')
  
  mbe = 0
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    if(model == "randomForest"){
      est = as.formula(paste(substitute(y), " ~ ."))
      set.seed(142)
      rf = randomForest(est, data = main_data[-(row_num), ])
      predictor = iml::Predictor$new(rf, type = "prob", data = main_data[-(row_num), ])
    }
    
    if(model == "log-reg"){
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
    }
    
    
    pred_x_interest = predictor$predict(x_interest)
    
    idx_pred = which(pred_x_interest>=0.5)
    name_col = names(pred_x_interest[idx_pred])
    
    fairness_obj = moccf::FairnessTest$new(predictor, df = main_data, sensitive_attribute = sen_attribute, n_generations = n_generation)
    fairness_obj$generate_counterfactuals(x_interest, desired_level = desired_level, desired_prob = desired_prob, fixed_features = fixed_features)
    difference = fairness_obj$get_prediction_difference(x_interest)
    endTime <- Sys.time()
    percentages = fairness_obj$prediction_percentages(x_interest)
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = name_col
    final_df[[3]][i] = pred_x_interest[, name_col]
    final_df[[4]][i] = fairness_obj$get_mpd()
    final_df[[5]][i] = fairness_obj$get_cfactuals_count()
    final_df[[6]][i] = round((endTime - startTime),2)
    final_df[[7]][i] = abs(final_df[[4]][i])
    
    percentage = fairness_obj$prediction_percentages(x_interest)
    if(i==1){
      name1 = names(percentage)[1]
      name2 = names(percentage)[2]
      final_df[name1] <- NA
      final_df[name2] <- NA
    }
    
    final_df[[8]][i] = percentage[[1]]
    final_df[[9]][i] = percentage[[2]]
    
    mbe = mbe + final_df[[7]][i]
  }
  mbe = abs(mbe)/nrow(df)
  # print("MBE for instances:")
  # print(mbe)
  return(list(as.data.frame(mbe), as.data.frame(final_df)))
  
}

#' fairnesstest_nice
#' 
#' A function to print the MBE and instance(s) with ampd for nice
#'
#' @return A character vector
#' @export

fairnesstest_nice = function(main_data, df, y, sen_attribute, desired_level, n_generation, desired_prob, model){
  final_df <- data.frame(matrix(ncol = 7, nrow = nrow(df)))
  colnames(final_df) <- c('instance_id', 'pred_x_interest', 'pred_prob_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)', 'ampd')
  
  mbe = 0
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    if(model == "randomForest"){
      est = as.formula(paste(substitute(y), " ~ ."))
      set.seed(142)
      rf = randomForest(est, data = main_data[-(row_num), ])
      predictor = iml::Predictor$new(rf, type = "prob", data = main_data[-(row_num), ])
    }
    
    if(model == "log-reg"){
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
    }
    
    # generating counterfactuals with NICE classifier
    est_protected = as.formula(paste(substitute(sen_attribute), " ~ ."))
    set.seed(142)
    rf_protected = randomForest(est_protected, data = main_data[-(row_num), ])
    predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = main_data[-(row_num), ])
    nice_classif = NICEClassif$new(predictor_protected)
    cfactuals = nice_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
    
    endTime <- Sys.time()
    
    
    data_archive = nice_classif$archive
    len = length(data_archive)
    cf_data = as.data.frame(data_archive[len])
    data_cfactuals <- cf_data %>% filter(cf_data[desired_level] > 0.5)
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
    
    idx_pred = which(pred_x_interest>=0.5)
    name_col = names(pred_x_interest[idx_pred])
    
    df_merged$diff_from_instance = (pred_x_interest[, name_col]) - (df_merged[, ..name_col])
    
    diff = df_merged$diff_from_instance
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = name_col
    final_df[[3]][i] = pred_x_interest[, name_col]
    final_df[[4]][i] = mean(diff)
    
    final_df[[5]][i] = nrow(data_cfactuals)
    final_df[[6]][i] = round((endTime - startTime),2)
    final_df[[7]][i] = abs(final_df[[4]][i])
    
    level = names(pred_x_interest)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))
    new_data[[level[1]]] <- ifelse(new_data[[level[1]]] >= 0.5, 1, 0)
    new_data[[level[2]]] <- ifelse(new_data[[level[2]]] >= 0.5, 1, 0)
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
    final_df[[8]][i] = d[1]
    final_df[[9]][i] = d[2]
    
    mbe = mbe + final_df[[7]][i]
  }
  mbe = mbe/nrow(df)
  # print("MBE for instances:")
  # print(mbe)
  
  return(list(as.data.frame(mbe), as.data.frame(final_df)))
  
}

#' fairnesstest_whatif
#' 
#' A function to print the MBE and instance(s) with ampd for whatif
#'
#' @return A character vector
#' @export
fairnesstest_whatif = function(main_data, df, y, sen_attribute, desired_level, desired_prob, model){
  final_df <- data.frame(matrix(ncol = 7, nrow = nrow(df)))
  colnames(final_df) <- c('instance_id', 'pred_x_interest', 'pred_prob_x_interest', 'mean_prediction_difference', 'total_cf', 'execution_time(s)', 'ampd')
  
  mbe = 0
  
  for (i in 1:nrow(df)){
    startTime <- Sys.time()
    row_num = as.integer(row.names(match_df(main_data, df[i,])))
    x_interest = main_data[row_num, ]
    
    if(model == "randomForest"){
      est = as.formula(paste(substitute(y), " ~ ."))
      set.seed(142)
      rf = randomForest(est, data = main_data[-(row_num), ])
      predictor = iml::Predictor$new(rf, type = "prob", data = main_data[-(row_num), ])
    }
    
    if(model == "log-reg"){
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
    }
    
    # generating counterfactuals with Whatif classifier
    est_protected = as.formula(paste(substitute(sen_attribute), " ~ ."))
    set.seed(142)
    rf_protected = randomForest(est_protected, data = main_data[-(row_num), ])
    predictor_protected = iml::Predictor$new(rf_protected, type = "prob", data = main_data[-(row_num), ])
    whatif_classif = WhatIfClassif$new(predictor_protected)
    cfactuals = whatif_classif$find_counterfactuals(x_interest = x_interest, desired_class = desired_level, desired_prob = desired_prob)
    
    endTime <- Sys.time()
    
    data_cfactuals = as.data.frame(cfactuals$data)
    
    idx_y_x = which(data.frame(colnames(x_interest)) == y)
    x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
    idx_y = which(data.frame(colnames(data_cfactuals)) == y)
    data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))
    
    data_cfactuals[sen_attribute] = desired_level
    data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
    data_cfactuals = data_cfactuals[-1,]
    
    pred_x_interest = predictor$predict(x_interest)
    pred_cfactuals = predictor$predict(data_cfactuals)
    
    df_merged = cbind(data_cfactuals, pred_cfactuals)
    
    idx_pred = which(pred_x_interest>=0.5)
    name_col = names(pred_x_interest[idx_pred])
    
    df_merged$diff_from_instance = (pred_x_interest[, name_col]) - (df_merged[, ..name_col])
    diff = df_merged$diff_from_instance
    
    final_df[[1]][i] = row_num
    final_df[[2]][i] = name_col
    final_df[[3]][i] = pred_x_interest[, name_col]
    final_df[[4]][i] = mean(df_merged$diff_from_instance)
    final_df[[5]][i] = nrow(data_cfactuals)
    final_df[[6]][i] = round((endTime - startTime),2)
    final_df[[7]][i] = abs(final_df[[4]][i])
    
    level = names(pred_x_interest)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))
    new_data[[level[1]]] <- ifelse(new_data[[level[1]]] >= 0.5, 1, 0)
    new_data[[level[2]]] <- ifelse(new_data[[level[2]]] >= 0.5, 1, 0)
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
    final_df[[8]][i] = d[1]
    final_df[[9]][i] = d[2]
    
    print(final_df[[7]][i])
    mbe = mbe + final_df[[7]][i]
    
  }
  mbe = abs(mbe)/nrow(df)
  # print("MBE for instances:")
  # print(mbe)
  return(list(as.data.frame(mbe), as.data.frame(final_df)))
}

