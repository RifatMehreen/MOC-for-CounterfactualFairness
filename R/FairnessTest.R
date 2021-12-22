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
     
     # dropping the response variable 
     # y = predictor$data$y.names
     # x_int = x_int[,!(names(x_int) %in% y)]
     
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
     # dataframe = dataframe[,!(names(dataframe) %in% y)]
     dataframe[sensitive_attribute] = desired_level
     dataframe = rbind(x_interest , dataframe)
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
     df_merged = df_merged[as.vector(df_merged[[idx_col]]) > 0.5, ]
     
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
     cf_data = private$get_counterfactuals()
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
   prediction_percentages = function(){
     cf_data = private$get_counterfactuals()
     new_data = cbind(cf_data, as.data.frame(self$predictor$predict(cf_data)))
     new_data <- subset(new_data, select = -c(9))
     new_data$two_year_recid <- ifelse(new_data$No > 0.5,"No", "Yes")
     prediction_of_cfs = levels(new_data$two_year_recid)
     percent_cf <- vector(mode = "list", length = 0)
     percent_cf$No = 100 * (sum(new_data$two_year_recid == "No")/nrow(new_data))
     percent_cf$Yes = 100 * (sum(new_data$two_year_recid == "Yes")/nrow(new_data))
     df = data.frame(No = numeric(0), Yes = numeric(0))
     newrow = data.frame(No=percent_cf$No, Yes=percent_cf$Yes)
     df <- rbind(df, newrow)
     return(df)
     
     # ggplot(new_data, aes(x=prediction_of_counterfactuals)) + geom_bar()
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
   # inside the function: add another attribute which we can input...
   plot_tSNE = function(x_interest){
     predictor = self$predictor
     df_data = as.data.frame(self$df)
     df_data["type"] = "original data"
     row_num = as.integer(row.names(match_df(df_data, x_interest)))
     df_data[row_num, ]$type = "x_interest"
     cf_data = private$get_counterfactuals()
     cf_data["type"] = "counterfactuals"
     
    
     df_merged = rbind(cf_data, df_data)
     df_merged <- df_merged %>% distinct()
     
     df_merged <- df_merged %>%
       drop_na() %>%
       dplyr::mutate(ID=row_number())
     
     recidivism_meta <- df_merged %>%
       select(ID, type, two_year_recid, race, sex)
     
     y = predictor$data$y.names
     task = mlr3::TaskClassif$new("task", df_merged, y)
     poe = po("encode")
     df_data = as.data.frame(poe$train(list(task))[[1]]$data())
     
     set.seed(142)
     tSNE_fit <- df_data %>%
       select(where(is.numeric)) %>%
       column_to_rownames("ID") %>%
       scale() %>%
       Rtsne::Rtsne(check_duplicates = FALSE)
     
     tSNE_df <- tSNE_fit$Y %>%
       as.data.frame() %>%
       dplyr::rename(tSNE1="V1",
              tSNE2="V2") %>%
       dplyr::mutate(ID=row_number())
     
     tSNE_df <- tSNE_df %>%
       dplyr::inner_join(recidivism_meta, by="ID")
     
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
    },
    
    #' @description 
    #' returns the predictor for protected attribute 
    #' 
    #' @return the predictor protected
    get_predictor_protected = function(predictor, x_interest){
      df = self$df
      row_num = as.integer(row.names(match_df(df, x_interest)))
      sensitive_attribute = self$sensitive_attribute
      # y = predictor$data$y.names
      # idx_y = which(data.frame(colnames(df)) == y)
      # df <- subset(df, select = -c(idx_y))
      
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