#' Fairness testing of an ML model
#' 
#' 
#' @examples 
#' if (require("randomForest")) {
#'   # data pre-processing 
#'   compas <- fairml::compas
#'   compas <- compas %>% drop_na()
#'   compas <- compas %>% distinct()
#'   # Train a model
#'   set.seed(142)
#'   rf = randomForest(two_year_recid ~ ., data = compas[-17L, ])
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Create a FairnessTest object
#'   fairness_obj = FairnessTest$new(predictor, df = compas, sensitive_attribute = "race", n_generations = 175)
#'   # Generate plausible counterfactuals
#'   cfatuals = fairness_obj$generate_counterfactuals(x_interest, desired_level = "Caucasian", desired_prob = c(0.5,1))
#'   # Find differences of the prediction of counterfactuals and original instance 
#'   pred_diff = fairness_obj$get_prediction_difference(x_interest)
#'   # Print the results
#'   print(pred_diff)
#' }
#' 
#' 
#' @export
FairnessTest = R6::R6Class("FairnessTest",
  public = list(
   #' @description Create a new FairnessTest object.
   #' @template predictor
   #' @param df (`data_frame`)\cr
   #'   The dataset or dataframe
   #' @param sensitive_attribute (`string`)\cr
   #'   The name of the protected attribute.
   #' @param n_generations (`integerish(1)`)\cr  
   #'   The number of generations. Default is `175L`.
   #' @field predictor (\link[iml]{Predictor})\cr
   #'   The object (created with `iml::Predictor$new()`) holding the machine learning model and the data.
   #' @field df (`data_frame`)\cr
   #'   The dataset or dataframe for the prediction
   #' @field sensitive_attribute (`string`)\cr
   #'   The name of the sensitive attribute.
   #' @field n_generations (`integerish(1)`)\cr  
   #'   The number of generations. Default is `175L`.
   #' @field cfs (`data_frame`)\cr
   #'   A datframe containing counterfactuals (plausible)
   #' @field org_cfs (`data_frame`)\cr
   #'   A dataframe containing all counterfactuals
   #' @field pred_diff (`data_frame`)\cr
   #'   A dataframe containing the prediction differences 
   predictor = NULL,
   df = NULL,
   sensitive_attribute = NULL,
   cfs = NULL,
   org_cfs = NULL,
   n_generations = NULL,
   pred_diff = NULL,
   initialize = function(predictor = NULL, df = NULL, sensitive_attribute = NULL, n_generations = 175L) {
     assert_class(predictor, "Predictor")
     assert_data_frame(df)
     
     self$predictor <- predictor
     self$df <- df
     self$sensitive_attribute <- sensitive_attribute
     self$n_generations <- n_generations
     self$cfs <- NULL
     self$org_cfs <- NULL
     self$pred_diff <-NULL
     },

   #' @description 
   #' Creates a dataframe with the plausible counterfactuals
   #' @template x_interest
   #' @param desired_level (`character(1)` | `NULL`) \cr
   #'   The desired level of the protected attribute.
   #'   If `NULL` (default) then `predictor_protected$class` is taken.
   #' @param desired_prob (`numeric(1)` | `numeric(2)`) \cr
   #'   The desired predicted probability of the `desired_level`. It can be a numeric scalar or a vector with two
   #'   numeric values that specify a probability range. 
   #'   For hard classification tasks this can be set to `0` or `1`, respectively.
   #' @param fixed_features (`character()` | `NULL`)\cr  
   #'   Names of features that are not allowed to change. `NULL` (default) allows to change all features.
   #' @return A dataframe with the plausible counterfactuals
   generate_counterfactuals = function(x_interest, desired_level, desired_prob, fixed_features = NULL){
     predictor = self$predictor
     df = self$df
     sensitive_attribute = self$sensitive_attribute
     n_generations = self$n_generations
     
     # variable checks
     assert_integerish(n_generations, lower = 0, len = 1L)
     assert_names(fixed_features, subset.of = predictor$data$feature.names)
     assert_character(sensitive_attribute, len = 1L, any.missing = FALSE)
     assert_character(desired_level, len = 1L, any.missing = FALSE)
     
     # Checks x_interest
     assert_data_frame(x_interest, nrows = 1L)
     assert_names(names(x_interest), must.include = names(predictor$data$X))
     x_interest = setDT(x_interest)[, names(predictor$data$X), with = FALSE]
     if (any(sapply(x_interest, typeof) != sapply(predictor$data$X, typeof))) {
       stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
     }
     
     # Checks desired_prob
     assert_numeric(desired_prob, any.missing = FALSE, min.len = 1L,  max.len = 2L, lower = 0, upper = 1)
     if (length(desired_prob) == 1L) {
       desired_prob = c(desired_prob, desired_prob)
     }
     if (desired_prob[2L] < desired_prob[1L]) {
       stop("The lower bound of `desired_prob` cannot be greater than the upper bound.")
     }
     
     # predictor for the protected attribute as response variable
     predictor_protected = private$get_predictor_protected(predictor, x_interest)
     
     # Checks desired_level
     if (is.null(desired_level)) {
       if (is.null(predictor_protected$class)) {
         stop("If `predictor_protected$class` is `NULL`, then the `desired_level` must be specified.")
       }
       desired_level = predictor_protected$class
       message(sprintf("The `desired_level` was set to the `predictor_protected$class` which is %s.", desired_level))
     }
     
     # generating counterfactuals using `MOCClassif`
     cf = private$get_cfactuals_moc(x_interest, predictor_protected, desired_prob, desired_level, df, fixed_features, n_generations)
     cfactuals = cf$data
     cf_predict = cf$predict()
     self$org_cfs = nrow(cfactuals)
     
     # taking only those with prediction prob >= 0.5
     cfactuals = cfactuals[which(cf_predict[desired_level]>=0.5), ]
     
     # transform the counterfactuals into dataframe and appending the protected attribute to the dataframe
     dataframe = as.data.frame(cfactuals)
     dataframe[sensitive_attribute] = desired_level
     
     # without the response variable
     dataframe = private$get_dataframe_wo_response(predictor, x_interest, dataframe)
     self$cfs = dataframe
   },
   
   #' @description 
   #' Creates a dataframe with the differences of predictions
   #' @return A dataframe with the differences of predictions of the original instance and the counterfactuals
   get_prediction_difference = function(x_interest){
     dataframe = self$cfs
     predictor = self$predictor
     
     # Checks x_interest
     assert_data_frame(x_interest, nrows = 1L)
     assert_names(names(x_interest), must.include = names(predictor$data$X))
     x_interest = setDT(x_interest)[, names(predictor$data$X), with = FALSE]
     if (any(sapply(x_interest, typeof) != sapply(predictor$data$X, typeof))) {
       stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
     }
     
     set.seed(142)
     # predicting the original instance
     pred_x_interest = predictor$predict(x_interest)
     
     # predicting the generated counterfactuals
     set.seed(142)
     pred_cfactuals_protected = predictor$predict(dataframe)
     df_pred_prot = as.data.frame(pred_cfactuals_protected)
     
     # calculating the distances
     df_merged = cbind(dataframe, df_pred_prot)
     
     idx_pred = which(pred_x_interest>=0.5)
     name_col = names(pred_x_interest[idx_pred])
     
     df_merged$diff_from_instance =  (pred_x_interest[, name_col]) - (df_merged[, name_col])
     self$pred_diff = df_merged$diff_from_instance
     return(df_merged)
   },
   
   #' @description 
   #' Creates a plot of the distribution
   #' @param factor_variable (`string`)\cr  
   #'   name of another factor variable that we want to see in the plot
   plot_tSNE = function(x_interest, factor_variable = NULL){
     predictor = self$predictor
     df_data = as.data.frame(self$df)
     df_data["type"] = "original data"
     sen_attribute = self$sensitive_attribute
     
     y = predictor$data$y.names
     idx_y = which(data.frame(colnames(x_interest)) == y)
     x_interest_wo_tyr <- subset(x_interest, select = -c(idx_y))
     row_num = as.integer(row.names(match_df(df_data, x_interest_wo_tyr)))
     df_data[row_num, ]$type = "x_interest"
     cf_data = private$get_counterfactuals()
     cf_data["type"] = "counterfactuals"
     df_data <- subset(df_data, select = -c(idx_y))
     
     df_merged = rbind(cf_data, df_data)
     df_merged <- df_merged %>% distinct()
     df_merged$type = as.factor(df_merged$type)

     df_merged <- df_merged %>%
       drop_na() %>%
       dplyr::mutate(ID=row_number())

     recidivism_meta <- df_merged %>%
       select(c(ID, factor_variable, sen_attribute, type))

     df_merged <- df_merged %>% distinct()
     df_merged <- df_merged %>% drop_na()

     df_data = as.data.frame(model.matrix(~., df_merged))
     rownames(df_data) = NULL

     set.seed(142)
     tSNE_fit <- df_data %>%
       select(where(is.numeric)) %>%
       column_to_rownames("ID") %>%
       as.data.frame() %>%
       drop_na() %>%
       Rtsne::Rtsne(check_duplicates = FALSE)

     tSNE_df <- tSNE_fit$Y %>%
       as.data.frame() %>%
       dplyr::rename(tSNE1="V1",
                     tSNE2="V2") %>%
       dplyr::mutate(ID=row_number())

     tSNE_df <- tSNE_df %>%
       dplyr::inner_join(recidivism_meta, by="ID")

     idx = which(tSNE_df$type == "x_interest")

     library(ggplot2)
     # change the color to the discriminated feature
     # change the shape to the other variable
     if(!is.null(factor_variable)){
       tSNE_df %>%
       ggplot2::ggplot(aes(x = tSNE1, y = tSNE2,))+ geom_point(aes_string(shape=factor_variable, color=sen_attribute, size="type")) +
         scale_size_manual(values=c(8,1,10)) + geom_circle(aes(x0 = tSNE_df[idx,]$tSNE1, y0 = tSNE_df[idx,]$tSNE2, r = 3),
                                                          color="black", inherit.aes = FALSE) + theme_light(base_size=10)+ theme(legend.position="bottom")   
       
     }
     
     else{
       tSNE_df %>%
       ggplot2::ggplot(aes(x = tSNE1, y = tSNE2,))+ geom_point(aes_string(shape=sen_attribute, color=sen_attribute, size="type")) +
         scale_size_manual(values=c(8,1,10)) + geom_circle(aes(x0 = tSNE_df[idx,]$tSNE1, y0 = tSNE_df[idx,]$tSNE2, r = 3),
                                                          color="black", inherit.aes = FALSE) + theme_light(base_size=10) + theme(legend.position="bottom")

     }
   },
   
   #' @description 
   #' prints the prediction of the plausible counterfactuals
   get_prediction = function(){
     cf_data = private$get_counterfactuals()
     print("Counterfactuals generated by MOC: ")
     print(cf_data)
     print("Prediction for the counterfactuals: ")
     set.seed(142)
     return(self$predictor$predict(cf_data))
   },
   
   #' @description
   #' calculates the percentage for the predictions
   #'
   #' @return the percentage for the predictions of generated counterfactuals
   prediction_percentages = function(x_interest){
     cf_data = private$get_counterfactuals()
     new_data = cbind(cf_data, as.data.frame(self$predictor$predict(cf_data)))
     y = self$predictor$data$y.names
     y_prot_name = self$sensitive_attribute
     pred_x_interest = self$predictor$predict(x_interest)
     level = names(pred_x_interest)
     idx_y = which(colnames(new_data) == y_prot_name)
     new_data <- subset(new_data, select = -c(idx_y))
     
     new_data[level[1]] <- ifelse(new_data[level[1]] >= 0.5, 1, 0)
     new_data[level[2]] <- ifelse(new_data[level[2]] >= 0.5, 1, 0)
      
     percent_cf <- vector(mode = "list", length = 0)
     percent_cf$l1 = round(100 * (length(which(new_data[level[1]] == 1))/nrow(new_data)), 2)
     percent_cf$l2 = round(100 * (length(which(new_data[level[2]] == 1))/nrow(new_data)), 2)
     
     df = data.frame(name1 = numeric(0), name2 = numeric(0))
     newrow = data.frame(name1 = percent_cf$l1, name2 = percent_cf$l2)
     df <- rbind(df, newrow)
     colnames(df)[1] <- level[1]
     colnames(df)[2] <- level[2]
     return(df)
   },
   
   #' @description 
   #' returns mean of the prediction differences 
   #' 
   #' @return mean of the prediction diferences
   get_mpd = function(){
     data = self$pred_diff
     m = mean(data)
     m_rounded = round(m, digits = 2)
     return(m_rounded)
   },
   
   #' @description 
   #' Gives the total count of counterfactuals
   get_cfactuals_count = function(){
     data_cfs = self$cfs
     return(nrow(data_cfs))
   }
  ),
  
  private = list(
    get_counterfactuals = function(){
      cf_data = self$cfs
      cf_data = as.data.frame(cf_data)
      return(cf_data)
    },
  
    get_predictor_protected = function(predictor, x_interest){
      df = self$df
      row_num = as.integer(row.names(match_df(df, x_interest)))
      sensitive_attribute = self$sensitive_attribute
      
      # deleting the `response` column from the prediction
      y = predictor$data$y.names
      idx_y = which(data.frame(colnames(df)) == y)
      df <- subset(df, select = -c(idx_y))
      
      est = as.formula(paste(substitute(sensitive_attribute), " ~ ."))
      set.seed(142)
      rf1 = randomForest(est, data = df[-(row_num), ])
      predictor_protected = iml::Predictor$new(rf1, type = "prob", data = df[-(row_num), ])
      return(predictor_protected)
    },
    
    get_cfactuals_moc = function(x_interest, predictor_protected, desired_prob, desired_level, df, fixed_features = NULL, n_generations){
      # we fixed the epsilon to zero
      moc_classif = counterfactuals::MOCClassif$new(predictor_protected, fixed_features = fixed_features, n_generations = n_generations, epsilon = 0)
      cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = desired_level, desired_prob = desired_prob)
    },
    
    get_dataframe_wo_response = function(predictor, x_interest, dataframe){
      y = predictor$data$y.names
      idx_y = which(data.frame(colnames(x_interest)) == y)
      x_interest_wo_r <- subset(x_interest, select = -c(idx_y))
      
      dataframe = rbind(x_interest_wo_r , dataframe)
      dataframe = dataframe[-1,]
      return(dataframe)
    }
  )
)