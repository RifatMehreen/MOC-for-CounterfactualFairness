#' Fairness testing of an ML model
#' 
#' @template FairnessTest
#' 
#' @examples 
#' if (require("randomForest")) {
#'   # data pre-processing 
#'   compas <- fairness::compas
#'   compas <- subset(compas, select = -c(8, 9))
#'   colnames(compas)[which(names(compas) == "Female")] <- "Gender"
#'   # Train a model
#'   rf = randomForest(Two_yr_Recidivism ~ ., data = compas[-900L, ])
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Find differences of the prediction of counterfactuals and original instance 
#'   fairness_obj = FairnessTest$new(predictor, df = compas, column = "ethnicity", row_num = 900L, desired_class = "Caucasian")
#'   # Print the results
#'   difference = fairness_obj$get_difference()
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
   #' @param column (string)\cr
   #' The name of protected column.
   #' @param row_num (numeric())\cr
   #' The row number of instance of interest `x_interest`.
   #' @param desired_class (string)\cr
   #' The desired class we want to have in our counterfactuals
   
   predictor = NULL,
   df = NULL,
   column = NULL,
   row_num = NULL,
   desired_class = NULL,
   data_tSNE = NULL,
   
   initialize = function(predictor = NULL, df = NULL, column = NULL, row_num = NULL, desired_class = NULL, epsilon = NULL, fixed_features = NULL, max_changed = NULL, mu = 20L, 
                         n_generations = 175L, p_rec = 0.57, p_rec_gen = 0.85, p_rec_use_orig = 0.88, p_mut = 0.79, 
                         p_mut_gen = 0.56, p_mut_use_orig = 0.32, k = 1L, weights = NULL, lower = NULL, upper = NULL, 
                         init_strategy = "random", use_conditional_mutator = FALSE) {
     
     super$initialize(predictor, epsilon, fixed_features, max_changed, mu, n_generations, p_rec, p_rec_gen, p_rec_use_orig, p_mut, 
                      p_mut_gen, p_mut_use_orig, k, weights, lower, upper, init_strategy, use_conditional_mutator)
     
     self$predictor <- predictor
     self$df <- df
     self$column <- column
     self$row_num <- row_num
     self$desired_class <- desired_class
     self$data_tSNE <- NULL
     },

   #' @description 
   #' Creates a dataframe with the differences of predictions
   #' 
   #' @return A dataframe with the differences of predictions of the original instance and the counterfactuals
   
   get_difference = function(){
     predictor = self$predictor
     df = self$df
     row_num = self$row_num
     desired_class = self$desired_class
     column = self$column
     
     # variable checks
     assert_number(row_num, lower = 1, upper = nrow(df))
     assert_character(column, len = 1L, any.missing = FALSE)
     assert_character(desired_class, len = 1L, any.missing = FALSE)
     
     # predictor for the protected attribute as response variable
     est = as.formula(paste(substitute(column), " ~ ."))
     rf1 = randomForest(est, data = df[-(row_num), ])
     predictor_protected = iml::Predictor$new(rf1, type = "prob", data = df[-(row_num), ])
     
     # creating a new object of `MOCClassif` for generating counterfactuals
     moc_classif = MOCClassif$new(predictor_protected, n_generations = 175L)
     x_interest = df[row_num, ]
     cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = desired_class, desired_prob = c(0.5, 1))
     
     # transform the counterfactuals into dataframe and appending the protected attribute to the dataframe
     dataframe = as.data.frame(cfactuals$data)
     dataframe[column] = desired_class
     dataframe = rbind(x_interest , dataframe)
     dataframe = dataframe[-1,]
     
     # predicting the original instance
     pred_x_interest = predictor$predict(x_interest)
     
     # predicting the generated counterfactuals
     pred_cfactuals_protected = predictor$predict(dataframe)
     df_pred_prot = as.data.frame(pred_cfactuals_protected)
     
     # calculating the distances
     df_merged = cbind(dataframe, df_pred_prot)
     
     # these are for tSNE plot
     self$data_tSNE = df_merged
     self$data_tSNE$two_year_recid = "No"
    
     # this `no` is for column name of probability of no.  
     #col_num = match("no", names(df_merged))
     #print(col_num)
     df_merged$diff_from_instance = ((df_merged[, "No"]) - (pred_x_interest[, 2]))
     df_merged = subset(df_merged, No >= 0.5)
     df_merged = df_merged[order(-No),]
     
     # deleting the response variable from the data frame
     n = grep(names(predictor$data$y), colnames(df_merged))
     df_final = subset(df_merged, select = -c(n) )

     # return the dataframe with distance
     return(df_final)
   },
  
   #' @description 
   #' returns a dataframe with the required column for tSNE plot
   #' 
   #' @return A dataframe of counterfactuals with the required column for tSNE plot

   get_dataset_for_tSNE = function(){
     data_tSNE = self$data_tSNE
     return(data_tSNE)
   }
  )
)