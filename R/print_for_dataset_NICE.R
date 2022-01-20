print_for_dataset_NICE = function(main_data, df, y, sen_attribute, desired_level, n_generation, desired_prob){
  final_df <- data.frame(matrix(ncol = 3, nrow = nrow(df)))
  colnames(final_df) <- c('counterfactuals_mean', 'total_cf', 'execution_time(s)')
  
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
    
    data_archive = nice_classif$archive
    len = length(data_archive)
    cf_data = as.data.frame(data_archive[len])
    # print("number of generated counterfactuals")
    # print(nrow(cf_data))
    # print(cf_data)
    data_cfactuals <- cf_data %>% filter(cf_data[desired_level] >= 0.5)
    data_cfactuals = as.data.frame(data_cfactuals)
    
    idx_y_x = which(data.frame(colnames(x_interest)) == y)
    x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y_x))
    idx_y = which(data.frame(colnames(data_cfactuals)) == y)
    data_cfactuals <- subset(data_cfactuals, select = -c(idx_y))
    
    # print("ei je rewaerd ase naki nai bujhtesina baa")
    # print(data_cfactuals)
    
    data_cfactuals <- subset(data_cfactuals, select = -c(grep("reward", colnames(data_cfactuals)):length(data_cfactuals)))
    data_cfactuals[sen_attribute] = desired_level
    print(x_interest_wo_tyr)
    print(data_cfactuals)
    data_cfactuals = rbind(x_interest_wo_tyr , data_cfactuals)
    data_cfactuals = data_cfactuals[-1,]
    
    # print("prediction probability for x_interest:")
    pred_x_interest = predictor$predict(x_interest)
    pred_cfactuals = predictor$predict(data_cfactuals)
    
    df_merged = cbind(data_cfactuals, pred_cfactuals)
    
    idx_pred = which(pred_x_interest<=0.5)
    name_col = names(pred_x_interest[idx_pred])
    idx_col = which(names(df_merged)==name_col)
    
    final_df[[1]][i] = mean(df_merged[[idx_col]])
    final_df[[2]][i] = nrow(data_cfactuals)
    final_df[[3]][i] = endTime - startTime
    
    level = names(pred_x_interest)
    # print(level)
    new_data = cbind(data_cfactuals, as.data.frame(predictor$predict(data_cfactuals)))
    idx_s = which(colnames(new_data) == sen_attribute)
    new_data <- subset(new_data, select = -c(idx_s))
    #return(new_data)
    new_data[[level[1]]] <- ifelse(new_data[[level[1]]] > 0.5, 1, 0)
    new_data[[level[2]]] <- ifelse(new_data[[level[2]]] > 0.5, 1, 0)
    percent_cf <- vector(mode = "list", length = 0)
    # return(new_data)
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
    #return(pred_cfactuals)
    final_df[[4]][i] = d[1]
    final_df[[5]][i] = d[2]
    
  }
  
  return(final_df)
    
  }