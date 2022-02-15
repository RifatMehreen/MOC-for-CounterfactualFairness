# $get_difference ------------------------------------------------------------------------------------------------
test_that("$get_difference returns a dataframe with all the desired attributes", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "prob")
  cc = CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL)
  x_interest = iris[1L, ]
  expect_snapshot_error(cc$find_counterfactuals(x_interest, desired_class = "setosa"))
})