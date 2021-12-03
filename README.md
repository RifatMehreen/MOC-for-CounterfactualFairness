# MOC-for-CounterfactualFairness

## Get started

In this example, we train a `randomForest` on the `iris` dataset.

We then examine how a given `virginica` observation would have to change
to be classified as `versicolor`.

``` r
library(counterfactuals)
library(randomForest)
library(iml)
```

First, we train the randomForest model to predict the `Species`. <br>
Note that we leave out one observation from the training data which is
our `x_interest`.

``` r
rf = randomForest(Species ~ ., data = iris[-150L, ])
```

We then create an `iml::Predictor` object, that holds the model and the
data for analyzing the model.

``` r
predictor = Predictor$new(rf, type = "prob")
```

Now we set up an object of the counterfactual explanation method we want
to use. Here we use `WhatIf` and since we have a classification task, we
create an `WhatIfClassif` object.

``` r
wi_classif = WhatIfClassif$new(predictor, n_counterfactuals = 5L)
```

For `x_interest` the model predicts:

``` r
x_interest = iris[150L, ]
predictor$predict(x_interest)
#>   setosa versicolor virginica
#> 1      0      0.104     0.896
```

We use the `$find_counterfactuals()` method to find counterfactuals for
`x_interest`.

``` r
cfactuals = wi_classif$find_counterfactuals(
  x_interest, desired_class = "versicolor", desired_prob = c(0.5, 1)
)
```

`cfactuals` is a `Counterfactuals` object that contains the found
counterfactuals and provides several methods for their evaluation and
visualization.

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
