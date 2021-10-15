#' Base class for Explanation Counterfactual fairness method 
#' 
#' @description 
#' Abstract base class for counterfactual fairness method.
#' 
#' @section Inheritance:
#' Child classes: \link{CounterfactualFairnessMethodClassif}
CounterfactualFairnessMethod = R6::R6Class("CounterfactualFairnessMethod",
                                   
  public = list(
   
   #' @description Creates a new `CounterfactualFairnessMethod` object.
   #' @template predictor
   #' @template lower_upper
   initialize = function(predictor, df = NULL, protected_column = NULL, row_num = NULL, predictor_protected = NULL, lower = NULL, upper = NULL) {
     assert_class(predictor, "Predictor")
     # assert_class(predictor_protected, "Predictor")
     assert_numeric(lower, null.ok = TRUE)
     assert_numeric(upper, null.ok = TRUE)
     assert_true(all(names(lower) %in% names(predictor$data$X)))
     assert_true(all(names(upper) %in% names(predictor$data$X)))
     print("inside the base init")
     
     # If the task could not be derived from the model, then we infer it from the prediction of some training data
     if (predictor$task == "unknown") {
       # Needs to be set to NULL, as the predictor does not infer the task from prediction otherwise
       # See: https://github.com/christophM/iml/blob/master/R/Predictor.R#L141 of commit 409838a.
       # The task is then checked by `CounterfactualFairnessMethodClassif`
        predictor$task = NULL
        predictor$predict(predictor$data$X[1:2, ])
     }
     
     #' Which predictor do I need to send here. The `make_param_set_fairness()` function is in 
     #' make_param_set_fairness.R file
     #' TODO: Here the predictor should be the second one. Predictor2
     private$predictor = predictor
     # private$predictor_protected = predictor_protected
     private$param_set = make_param_set_fairness(predictor$data$X, lower, upper)
     private$lower = lower
     private$upper = upper
     
     # if(is.null(predictor_protected)) {
     #    print("predictor prot is NULL")
     #    est <- as.formula(paste(substitute(protected_column), " ~ ."))
     #    rf1 = randomForest::randomForest(est, data = df[-(row_num), ], na.action=na.omit)
     #    
     #    predictor = iml::Predictor$new(rf1, type = "prob", data = df[-(row_num)])
     #    private$predictor_protected = predictor_protected
     # }
   },
   
   #' @description 
   #' Print a `CounterfactualFairnessMethod` object.
   #' The method calls a (private) `$print_parameters()` method which should be implemented by the leaf classes.
   print = function() {
     cat("Counterfactual fairness method: ", class(self)[1], "\n")
     cat("Parameters:\n")
     private$print_parameters()
   },
   
   #' @description
   #' Get a predictor for the protected attribute as response variable.
   get_predictor_protected = function() stop("abstract")
  ),
  
  private = list(
   predictor = NULL,
   predictor_protected = NULL,
   x_interest = NULL,
   param_set = NULL,
   lower = NULL,
   upper = NULL,
   
   run = function() stop("abstract"),
   
   print_parameters = function() {}
  )
)


