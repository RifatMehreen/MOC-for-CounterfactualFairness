#' Fairness testing of an ML model
#' 
#' @template FairnessTest
#' 
#' @examples 
#' if (require("randomForest")) {
#'   # data pre-processing 
#'   compas <- fairml::compas
#'   compas <- compas %>% drop_na()
#'   compas <- compas %>% distinct()
#'   # Train a model
#'   rf = randomForest(two_year_recid ~ ., data = compas[-17L, ])
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Find differences of the prediction of counterfactuals and original instance 
#'   fairness_obj = FairnessTest$new(predictor, df = compas, column = "race", row_num = 17L, desired_class = "Caucasian")
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
   x_int = NULL,
   
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
     self$x_int <- NULL
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
     set.seed(142)
     rf1 = randomForest(est, data = df[-(row_num), ])
     predictor_protected = iml::Predictor$new(rf1, type = "prob", data = df[-(row_num), ])
     
     # creating a new object of `MOCClassif` for generating counterfactuals
     moc_classif = MOCClassif$new(predictor_protected, n_generations = 175L)
     self$x_int = df[row_num, ]
     #x_interest = df[row_num, ]
     cfactuals = moc_classif$find_counterfactuals(self$x_int, desired_class = desired_class, desired_prob = c(0.5, 1))
     
     # transform the counterfactuals into dataframe and appending the protected attribute to the dataframe
     dataframe = as.data.frame(cfactuals$data)
     dataframe[column] = desired_class
     dataframe = rbind(self$x_int , dataframe)
     dataframe = dataframe[-1,]
     print(dataframe)
     # predicting the original instance
     pred_x_interest = predictor$predict(self$x_int)
     
     # predicting the generated counterfactuals
     pred_cfactuals_protected = predictor$predict(dataframe)
     df_pred_prot = as.data.frame(pred_cfactuals_protected)
     
     # calculating the distances
     df_merged = cbind(dataframe, df_pred_prot)
     
     # these are for tSNE plot
     self$data_tSNE = df_merged
     # self$data_tSNE$two_year_recid = "No"
    
     # this `no` is for column name of probability of no.  
     df_merged$diff_from_instance = ((df_merged[, "No"]) - (pred_x_interest[, 1]))
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
   
   print_prediction = function(){
     x_interest = self$x_int
     predictor = self$predictor
     cf_data = private$get_counterfactuals()
     print("x_interest: ")
     print(x_interest)
     print("prediction for x_interest: ")
     print(predictor$predict(x_interest))
     print("Counterfactuals generated by MOC: ")
     print(cf_data)
     print("prediction for the counterfactuals: ")
     print(as.data.frame(predictor$predict(cf_data)))
   },
   
   #' @description 
   #' prints the counterfactuals
   #' 
   #' @return prints the generated counterfactuals
   print_counterfactuals = function(){
     print(private$get_counterfactuals())
   },
   
   #' @description 
   #' plots a bar plot for the predictions
   #' 
   #' @return A barplot for the predictions
   plot_bar = function(){
     cf_data = private$get_counterfactuals()
     new_data = cbind(cf_data, as.data.frame(self$predictor$predict(cf_data)))
     new_data <- subset(new_data, select = -c(9))
     new_data$two_year_recid <- ifelse(new_data$No > 0.5,"No", "Yes")
     prediction_of_counterfactuals = new_data$two_year_recid
     ggplot(new_data, aes(x=prediction_of_counterfactuals)) + geom_bar()
   },
   
   #' @description 
   #' returns mean of the prediction of the counterfactuals as the opposite of the x_interest 
   #' 
   #' @return mean of the prediction of the counterfactuals as the opposite of the x_interest
   get_cfactuals_mean = function(){
     data_tSNE = self$data_tSNE
     return(mean(data_tSNE$No))
   },
   
   #' @description 
   #' plots tSNE
   #' 
   #' @return tSNE plot for the dataset with counterfactuals and x_interest
   plot_tSNE = function(){
     df_data = as.data.frame(self$df)
     df_data["type"] = "original data"
     row_num = self$row_num
     df_data[row_num, ]$type = "x_interest"
     cf_data = private$get_counterfactuals()
     cf_data["type"] = "counterfactuals"
     
    
     df_merged = rbind(cf_data, df_data)
     df_merged <- df_merged %>% distinct()
     
     df_merged <- df_merged %>%
       drop_na() %>%
       mutate(ID=row_number())
     
     recidivism_meta <- df_merged %>%
       select(ID, type, two_year_recid, race, sex)
     
     task = TaskClassif$new("task", df_merged, "two_year_recid")
     poe = po("encode")
     df_data = as.data.frame(poe$train(list(task))[[1]]$data())
     
     set.seed(142)
     tSNE_fit <- df_data %>%
       select(where(is.numeric)) %>%
       column_to_rownames("ID") %>%
       scale() %>%
       Rtsne(check_duplicates = TRUE)
     
     tSNE_df <- tSNE_fit$Y %>%
       as.data.frame() %>%
       rename(tSNE1="V1",
              tSNE2="V2") %>%
       mutate(ID=row_number())
     
     tSNE_df <- tSNE_df %>%
       inner_join(recidivism_meta, by="ID")
     
     tSNE_df %>% head()
     
     idx = which(tSNE_df$type == "x_interest")
     
     library(ggplot2)
     tSNE_df %>%
       ggplot2::ggplot(aes(x = tSNE1, y = tSNE2,))+ geom_point(aes(shape=sex, color=race, size=type)) + 
        scale_size_manual(values=c(3,1,4)) + geom_circle(aes(x0 = tSNE_df[idx,]$tSNE1, y0 = tSNE_df[idx,]$tSNE2, r = 2),
        color="green", inherit.aes = FALSE) + theme(legend.position="bottom")
     
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
      cf_data <- subset(cf_data, select = -c(17, 18))
      return(cf_data)
    }
  )
)