library(checkmate)

# $initialization ------------------------------------------------------------------------------------------------------
test_that("$initialize() returns error if predictor given does not have the correct class", {
  expect_snapshot_error(FairnessTest$new(predictor = "wrong", df = NULL, sensitive_attribute = NULL, n_generations = 175L))
})

test_that("$initialize() returns error if dataframe given does not have the correct class", {
  expect_snapshot_error(FairnessTest$new(predictor = "wrong", df = "wrong", sensitive_attribute = NULL, n_generations = 175L))
})

# $generate_counterfactuals ------------------------------------------------------------------------------------------------
test_that("$generate_counterfactuals returns meaningful error if x_interest does not contain all columns of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_classif_adult()
  pred_class = iml::Predictor$new(rf, type = "class", class = ">=50K")
  cc = FairnessTest$new(predictor = pred_class)
  
  expect_snapshot_error(cc$generate_counterfactuals(mtcars[1L, ]), desired_level = "Male", desired_prob = c(0.5,1), fixed_features = "race")
})

test_that("$generate_counterfactuals returns meaningful error if x_interest has unexpected column types", {
  set.seed(54542142)
  rf = get_rf_classif_adult()
  pred_class = iml::Predictor$new(rf, type = "class", class = ">=50K")
  cc = FairnessTest$new(predictor = pred_class)
  
  x_interest = adult[1L, ]
  x_interest$age = as.character(x_interest$age)
  expect_snapshot_error(cc$generate_counterfactuals(x_interest))
})

# $get_prediction_difference ------------------------------------------------------------------------------------------------

