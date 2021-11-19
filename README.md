# MOC-for-CounterfactualFairness

 ```r
 if (require("randomForest")) {
   # data pre-processing 
   compas <- fairml::compas
   compas <- compas %>% drop_na()
   compas <- compas %>% distinct()
   
   # Train a model
   rf = randomForest(two_year_recid ~ ., data = compas[-17L, ])
   
   # Create a predictor object
   predictor = iml::Predictor$new(rf, type = "prob")
   
   # Find differences of the prediction of counterfactuals and original instance 
   fairness_obj = FairnessTest$new(predictor, df = compas, column = "race", row_num = 17L, desired_class = "Caucasian")
   
   # Print the results
   difference = fairness_obj$get_difference()
   print(difference)
 }```
