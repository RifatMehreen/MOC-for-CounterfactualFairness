# $get_difference ------------------------------------------------------------------------------------------------
test_that("$get_difference returns a dataframe with all the desired attributes", {
  set.seed(142)
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "prob")
  cc = CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL)
  x_interest = iris[17L, ]
  expect_snapshot_error(cc$find_counterfactuals(x_interest, desired_class = "setosa"))
})

test_that("Can handle non-numeric target classes", {
  set.seed(544564)
  test_data = data.frame(a = rnorm(10), b = rnorm(10), cl = as.factor(rep(c("pos", "neg"), each = 5)))
  rf_pima = randomForest::randomForest(cl ~ . , test_data, ntree = 2L)
  pred = iml::Predictor$new(rf_pima, data = test_data, y = "cl")
  x_interest = head(subset(test_data, select = -cl), 1L)
  set.seed(544564)
  mocc = MOCClassif$new(pred, n_generations = 5L)
  expect_snapshot({
    cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "pos"))
  })
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

# $generate_counterfactuals ------------------------------------------------------------------------------------------------
test_that("$generate_counterfactuals returns meaningful error if x_interest does not contain all columns of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  cc = CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL)
  
  expect_snapshot_error(cc$find_counterfactuals(mtcars[1L, ]))
})