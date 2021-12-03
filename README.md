# MOC-for-CounterfactualFairness

In this example, we train a `randomForest` on the `COMPAS` dataset.

We now examine whether a given `chance of reoffending` observation will change
to a `no-chance of reoffending`.

``` r
library(counterfactuals)
library(randomForest)
library(iml)
```

First, we load the data and pre-process it

```r
# data pre-processing 
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


 # Print the results
 difference = fairness_obj$get_difference()
 print(difference)
