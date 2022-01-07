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
#'   # Find differences of the prediction of counterfactuals and original instance 
#'   fairness_obj = FairnessTest$new(predictor, df = compas, sensitive_attribute = "race", n_generations = 175)
#'   # Print the results
#'   difference = fairness_obj$get_difference(x_interest, desired_level = "Caucasian", desired_prob = c(0.5,1))
#'   print(difference)
#' }
#' 
#' 
#' @export
FairnessTest = R6::R6Class("FairnessTest", inherit = MOCClassif,
  
  public = list(
   #' @description Create a new FairnessTest object.
   #' @template predictor
   #' @param predictor (`iml predictor`)\cr
   #' The original predictor where as response variable we use `Two_yr_Recidivism`.
   #' @param df (dataframe)\cr
   #' The dataset or dataframe for the prediction
   #' @param sensitive_attribute (string)\cr
   #' The name of sensitive_attribute.
   #' @param row_num (numeric())\cr
   #' The row number of instance of interest `x_interest`.
   #' @param desired_level (string)\cr
   #' The desired class we want to have in our counterfactuals
   
   predictor = NULL,
   df = NULL,
   sensitive_attribute = NULL,
   data_tSNE = NULL,
   row_num = NULL,
   n_generations = NULL,
   
   initialize = function(predictor = NULL, df = NULL, sensitive_attribute = NULL, epsilon = NULL, fixed_features = NULL, max_changed = NULL, mu = 20L, 
                         n_generations = 175L, p_rec = 0.57, p_rec_gen = 0.85, p_rec_use_orig = 0.88, p_mut = 0.79, 
                         p_mut_gen = 0.56, p_mut_use_orig = 0.32, k = 1L, weights = NULL, lower = NULL, upper = NULL, 
                         init_strategy = "random", use_conditional_mutator = FALSE) {
     
     super$initialize(predictor, epsilon, fixed_features, max_changed, mu, n_generations, p_rec, p_rec_gen, p_rec_use_orig, p_mut, 
                      p_mut_gen, p_mut_use_orig, k, weights, lower, upper, init_strategy, use_conditional_mutator)
     
     self$predictor <- predictor
     self$df <- df
     self$sensitive_attribute <- sensitive_attribute
     self$n_generations <- n_generations
     self$data_tSNE <- NULL
     },

   #' @description 
   #' Creates a dataframe with the differences of predictions
   #' 
   #' @return A dataframe with the differences of predictions of the original instance and the counterfactuals
   
   get_difference = function(x_interest, desired_level, desired_prob){
     predictor = self$predictor
     df = self$df
     sensitive_attribute = self$sensitive_attribute
     n_generations = self$n_generations
     
     # variable checks
     # assert_number(row_num, lower = 1, upper = nrow(df))
     assert_character(sensitive_attribute, len = 1L, any.missing = FALSE)
     assert_character(desired_level, len = 1L, any.missing = FALSE)
     
     # predictor for the protected attribute as response variable
     predictor_protected = private$get_predictor_protected(predictor, x_interest)
     
     # generating counterfactuals using `MOCClassif`
     cfactuals = private$get_cfactuals_moc(x_interest, predictor_protected, desired_prob, desired_level, df, n_generations)
     
     # transform the counterfactuals into dataframe and appending the protected attribute to the dataframe
     dataframe = as.data.frame(cfactuals$data)
     dataframe[sensitive_attribute] = desired_level
     
     # without the response variable
     y = predictor$data$y.names
     idx_y = which(data.frame(colnames(x_interest)) == y)
     x_interest_wo_r <- subset(x_interest, select = -c(idx_y))
     
     dataframe = rbind(x_interest_wo_r , dataframe)
     dataframe = dataframe[-1,]
     
     # predicting the original instance
     pred_x_interest = predictor$predict(x_interest)
     
     # predicting the generated counterfactuals
     pred_cfactuals_protected = predictor$predict(dataframe)
     df_pred_prot = as.data.frame(pred_cfactuals_protected)
     
     # calculating the distances
     df_merged = cbind(dataframe, df_pred_prot)
     
     # this is for tSNE plot
     self$data_tSNE = df_merged
    
     # this `no` is for column name of probability of no.
     idx_pred = which(pred_x_interest<=0.5)
     name_col = names(pred_x_interest[idx_pred])
     df_merged$diff_from_instance = ((df_merged[, ..name_col]) - (pred_x_interest[, 1]))
     idx_col = which(names(df_merged)==name_col)
     
     # df_merged consists only of those whose prediction (for "No" here) is greater than 0.5
     df_merged = df_merged[as.vector(df_merged[[idx_col]]) > 0.5, ]

     # return the dataframe with distance
     return(df_merged)
   },
  
   #' @description 
   #' returns a dataframe with the required column for tSNE plot
   #' 
   #' @return A dataframe of counterfactuals with the required column for tSNE plot
   
   print_prediction = function(){
     cf_data = private$get_counterfactuals()
     print("Counterfactuals generated by MOC: ")
     print(cf_data)
     print("prediction for the counterfactuals: ")
     set.seed(142)
     print(as.data.frame(self$predictor$predict(cf_data)))
   },
   
   #' @description 
   #' prints the counterfactuals
   #' 
   #' @return prints the generated counterfactuals
   print_counterfactuals = function(){
     print(private$get_counterfactuals())
   },
   
   #' @description
   #' prints the percentage for the predictions
   #'
   #' @return the percentage for the predictions of generated counterfactuals
   prediction_percentages = function(x_interest){
     cf_data = private$get_counterfactuals()
     new_data = cbind(cf_data, as.data.frame(self$predictor$predict(cf_data)))
     y = self$predictor$data$y.names
     y_prot_name = self$sensitive_attribute
     pred_x_interest = self$predictor$predict(x_interest)
     level = names(pred_x_interest1)
     idx_y = which(colnames(new_data) == y_prot_name)
     new_data <- subset(new_data, select = -c(idx_y))
     #print(new_data)
     
     # new_data$two_year_recid <- ifelse(new_data$No > 0.5,"No", "Yes")
     new_data[y] <- ifelse(new_data[level[1]] > 0.5,"No", "Yes")
     print(new_data)
     
     # prediction_of_cfs = levels(new_data$two_year_recid)
     # percent_cf <- vector(mode = "list", length = 0)
     # percent_cf$No = 100 * (sum(new_data$two_year_recid == "No")/nrow(new_data))
     # percent_cf$Yes = 100 * (sum(new_data$two_year_recid == "Yes")/nrow(new_data))
     # df = data.frame(No = numeric(0), Yes = numeric(0))
     # newrow = data.frame(No=percent_cf$No, Yes=percent_cf$Yes)
     # df <- rbind(df, newrow)
     # return(df)
   },
   
   #' @description 
   #' returns mean of the prediction of the counterfactuals as the opposite of the x_interest 
   #' 
   #' @return mean of the prediction of the counterfactuals as the opposite of the x_interest
   #' *** need to change here.....
   get_cfactuals_mean = function(){
     data_tSNE = self$data_tSNE
     return(mean(data_tSNE$No))
   }
  ),
  
  private = list(
    #' @description 
    #' returns the counterfactuals 
    #' 
    #' @return the generated counterfactuals
    get_counterfactuals = function(){
      cf_data = self$data_tSNE
      cf_data = as.data.frame(cf_data)
      cf_data = cf_data[1:(length(cf_data)-2)]
      # cf_data <- subset(cf_data, select = -c(16, 17))
      return(cf_data)
    },
    
    #' @description 
    #' returns the predictor for protected attribute 
    #' 
    #' @return the predictor protected
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
    
    #' @description 
    #' creating a new object of `MOCClassif` for generating counterfactuals
    #' 
    #' @return the moc generated cfactuals
    get_cfactuals_moc = function(x_interest, predictor_protected, desired_prob, desired_level, df, n_generations){
      # we fixed the epsilon to zero
      moc_classif = MOCClassif$new(predictor_protected, n_generations = n_generations, epsilon = 0)
      cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = desired_level, desired_prob = desired_prob)
    }
  )
)