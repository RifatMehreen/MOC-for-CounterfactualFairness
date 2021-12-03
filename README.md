# MOC-for-CounterfactualFairness

In this example, we train a `randomForest` on the `COMPAS` dataset.

We now examine whether a given `yes` for `two_yr_recid` observation will change
to a `no` for the generated counterfactuals.

``` r
library(counterfactuals)
library(randomForest)
library(iml)
```

First, we load the data and pre-process it

```r
compas <- fairml::compas
compas <- compas %>% drop_na()
compas <- compas %>% distinct()
``` 
 
Then we train the randomForest model to predict the `two-yr-recid` (Two years recidivism or chances of reoffending). <br>
Note that we leave out one observation from the training data which is
our `x_interest`.

``` r
rf = randomForest(two_year_recid ~ ., data = compas[-17L, ])
```

We now create an `iml::Predictor` object, that holds the model and the
data for analyzing the model.

``` r
predictor = iml::Predictor$new(rf, type = "prob")
```

Now we set up an object of the counterfactual fairness test method we want
to use. Here we use `FairnessTest`

``` r
fairness_obj = FairnessTest$new(predictor, df = compas, column = "race", row_num = 17L, desired_class = "Caucasian", n_generations = 100)
```

For `x_interest` the model predicts:

``` r
> x_interest = compas[17L, ]
> predictor$predict(x_interest)
#>    No  Yes
#> 1 0.23 0.77
```

We use the `$get_difference()` method to find difference of predictions of the  `x_interest` and the counterfactuals.

``` r
difference = fairness_obj$get_difference()
```

We can also use `$print_prediction()` method to look into the predictions for both `x_interest` and the generated counterfactuals
``` r
fairness_obj$print_prediction()
```

Use `$print_counterfactuals()` to get the generated counterfactuals
``` r
fairness_obj$print_counterfactuals()
```

There are also some other methods for visualizationa and evaluation

