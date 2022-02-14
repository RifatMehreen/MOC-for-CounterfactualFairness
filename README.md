# MOC-for-CounterfactualFairness

In this example, we train a `randomForest` on the `COMPAS` dataset.

We now examine whether a given `yes` for `two_yr_recid` observation will change
to a `no` for the generated counterfactuals (with `race` changed to `Caucasian` from `African-American`).

``` r
library(dplyr)
library(plyr)
library(tidyverse)
library(Rtsne)
library(mlr3pipelines)
library(mlr3learners)
options(rgl.useNULL = TRUE)
library(rgl)
library(Rmpfr)
library(checkmate)
library(R6)
library(paradox)
library(data.table)
library(miesmuschel)
library(fairml)
library(counterfactuals)
library(randomForest)
library(iml)
library(ggforce)
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
fairness_obj = FairnessTest$new(predictor, df = compas, sensitive_attribute = "race", n_generations = 175)
```

For `x_interest` the model predicts:

``` r
> x_interest = compas[17L, ]
> predictor$predict(x_interest)
#>    No  Yes
#> 1 0.24 0.76
```

First we generate counterfactuals by running `generate_countefactuals()`:

``` r
> cfactuals = fairness_obj$generate_counterfactuals(x_interest, desired_level = "Caucasian", desired_prob = c(0.5,1))
> cfactuals
#>    age juv_fel_count decile_score juv_misd_count juv_other_count v_decile_score priors_count  sex      race c_jail_in
#> 1:  21     0.0000000     9.000000              0        4.189173              9     1.000000 Male Caucasian 0.7913651
#> 2:  21     0.0000000     7.409944              0        3.685493              9     2.949364 Male Caucasian 0.7913651
#> 3:  21     0.4692705     9.000000              0        3.856240              9     1.000000 Male Caucasian 0.7913651
#> 4:  21     0.0000000     5.254366              0        3.391174              9     1.000000 Male Caucasian 0.7913651
      c_jail_out c_offense_date screening_date in_custody out_custody
#> 1:  0.7916205      0.7374175      0.7914502  0.7940049   0.7940901
#> 2:  0.7916205      0.7913651      0.7871658  0.7940049   0.7940901
#> 3:  0.7942515      0.7707731      0.7897707  0.7927961   0.7959850
#> 4:  0.7916205      0.7913651      0.7914502  0.7940049   0.7940901
```

We use the `$get_prediction_difference()` method to find difference of predictions of the  `x_interest` and the counterfactuals.

``` r
difference = fairness_obj$get_prediction_difference()(x_interest, desired_level = "Caucasian", desired_prob = c(0.5,1))
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

