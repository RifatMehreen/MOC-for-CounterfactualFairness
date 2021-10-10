#' Base class for Counterfactual Fairness Methods for Classification Tasks
#' 
#' @description 
#' Abstract base class for counterfactual fairness methods for classifcation task.
#' 
#' `CounterfactualFairnessMethodClassif` can only be initialized for classification tasks. Child classes inherit the (public) 
#' `$find_counterfactuals()` method, which calls a (private) `$run()` method. This `$run()` method should be implemented 
#' by the child classes and return the counterfactuals as `data.table` (preferably) or `data.frame`.
#' 
#' @section Inheritance:
#' Child classes: \link{MOCClassif}
#'  
#' @export
CounterfactualFairnessMethodClassif = R6::R6Class("CounterfactualFairnessMethodClassif", inherit = CounterfactualFairnessMethod,
  
  public = list(
    
    #' @description Creates a new `CounterfactualFairnessMethodClassif` object.
    #' @template predictor
    #' @template lower_upper
    initialize = function(predictor, lower = NULL, upper = NULL) {
      super$initialize(predictor, lower, upper)
      if (private$predictor$task != "classification") {
        stop(sprintf("%s only works for classification tasks.", class(self)[1]))
      }
    },
    
    #' @description 
    #' 
    #' Runs the counterfactual fairness method and returns the counterfactuals.
    #' It searches for counterfactuals that have the `desired_protected_attribute` with a probability `desired_prob`.
    #' 
    #' @template x_interest
    #' @param desired_protected_attribute (`character(1)` | `NULL`) \cr
    #'   # TODO
    #'   The desired class. If `NULL` (default)
    #' @param desired_prob (`numeric(1)` | `numeric(2)`) \cr
    #'   The desired probability of the `desired_protected_attribute`. It can be a numeric scalar or a vector with two
    #'   numeric values that specify a probability range.
    #'   
    #' @return A \link{Counterfactuals} object containing the results.
    find_counterfactuals = function(x_interest, protected_attribute_col_name = NULL, desired_protected_attribute = NULL, desired_prob = c(0.5, 1)) {
      # Checks x_interest
      assert_data_frame(x_interest, nrows = 1L)
      assert_names(names(x_interest), must.include = names(private$predictor$data$X))
      x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
      if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
        stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
      }
      
      # Checks column name of the Protected attribute
      # ----------------------------
      
      # Checks desired_prob
      assert_numeric(desired_prob, any.missing = FALSE, min.len = 1L,  max.len = 2L, lower = 0, upper = 1)
      if (length(desired_prob) == 1L) {
        desired_prob = c(desired_prob, desired_prob)
      }
      if (desired_prob[2L] < desired_prob[1L]) {
        stop("The lower bound of `desired_prob` cannot be greater than the upper bound.")
      }
      
      # Checks desired_protected_attribute
      
      if (is.null(desired_protected_attribute)) {
        stop("The `desired_protected_attribute` is `NULL`, it must be specified.")
      }
      assert_character(desired_protected_attribute, len = 1L, any.missing = FALSE)
      # y_hat_interest = private$predictor$predict(x_interest)
      idx = which(names(predictor$data$X) == protected_attribute_col_name)
      
      # maybe at the starting make a list of protected attributes for specific datasets? (gender, ethnicity, age, citizenship, national_origin?)
      # need to write ethnicity here
      assert_choice(desired_protected_attribute, choices = levels(predictor$data$X[[idx]]))
      # if (between(y_hat_interest[[desired_class]], desired_prob[[1L]], desired_prob[[2L]])) {
      #  stop("`x_interested` is already predicted with `desired_prob` for `desired_class`.")
      # }
      
      private$x_interest = x_interest
      private$protected_attribute_col_name = protected_attribute_col_name
      private$desired_protected_attribute = desired_protected_attribute
      private$desired_prob = desired_prob
      cfactuals = private$run()
      
      if (is.data.frame(cfactuals) && nrow(merge(cfactuals, x_interest)) > 0L) {
        cfactuals = cfactuals[!x_interest, on = names(cfactuals)]
        message("`x_interest` was removed from results.")
      }
      
      Counterfactuals_fairness$new(
        cfactuals = cfactuals, 
        predictor = private$predictor,
        x_interest = private$x_interest, 
        param_set = private$param_set,   
        desired = list("protected_attribute_col_name" = protected_attribute_col_name, "desired_protected_attribute" = desired_protected_attribute, "desired_prob" = desired_prob)
      )
    }
  ),
  
  private = list(
    desired_prob = NULL,
    desired_protected_attribute = NULL,
    protected_attribute_col_name = NULL,
    
    # TODO: investigate what happens 
    get_pred_column = function() {private$desired_protected_attribute}
  )
)

