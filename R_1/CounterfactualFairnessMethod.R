#' Abstract base class for the explanation of Counterfactual Fairness methods

# - new() Abstract, does nothing
# - run()  - "
# - print, ...

#' Base class for Explanation Counterfactual fairness method 
#' 
#' @description 
#' Abstract base class for counterfactual fairness method.
#' 
#' @section Inheritance:
#' Child classes: \link{CounterfactualMethodClassif}, \link{CounterfactualMethodRegr}
CounterfactualFairnessMethod = R6::R6Class("CounterfactualFairnessMethod",
                                   
  public = list(
   
   #' @description Creates a new `CounterfactualFairnessMethod` object.
   #' @template predictor
   #' @template lower_upper
   initialize = function(predictor, lower = NULL, upper = NULL) {
     assert_class(predictor, "Predictor")
     assert_numeric(lower, null.ok = TRUE)
     assert_numeric(upper, null.ok = TRUE)
     assert_true(all(names(lower) %in% names(predictor$data$X)))
     assert_true(all(names(upper) %in% names(predictor$data$X)))
     
     # If the task could not be derived from the model, then we infer it from the prediction of some training data
     if (predictor$task == "unknown") {
       # Needs to be set to NULL, as the predictor does not infer the task from prediction otherwise
       # See: https://github.com/christophM/iml/blob/master/R/Predictor.R#L141 of commit 409838a.
       # The task is then checked by `CounterfactualMethodRegr` or `CounterfactualMethodClassif`
       predictor$task = NULL
       predictor$predict(predictor$data$X[1:2, ])
     }
     
     private$predictor = predictor
     private$param_set = make_param_set(predictor$data$X, lower, upper)
     private$lower = lower
     private$upper = upper
   },
   
   #' @description 
   #' Print a `CounterfactualFairnessMethod` object.
   #' The method calls a (private) `$print_parameters()` method which should be implemented by the leaf classes.
   print = function() {
     cat("Counterfactual fairness method: ", class(self)[1], "\n")
     cat("Parameters:\n")
     private$print_parameters()
   }
  ),
  
  private = list(
   predictor = NULL,
   x_interest = NULL,
   param_set = NULL,
   lower = NULL,
   upper = NULL,
   
   run = function() stop("abstract"),
   
   print_parameters = function() {}
  )
)


