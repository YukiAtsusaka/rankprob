
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rankprob

<!-- badges: start -->
<!-- badges: end -->

`rankprob` facilitates the computation of the predicted probability of a
given ranking of choices from the output of a multinomial regression
model, in particular, making it easy to use these predicted
probabilities in `marginaleffects` and `clarify`. It does so by
providing a new type of prediction for multinomial models, which is
activated by running `convert_rankprob()` on the multinomial or ordinal
regression output object, e.g., the output of a call to
`nnet::multinom()`, `MASS::polr()`, or `WeightIt::multinom_weightit()`.

## Installation

You can install the development version of `rankprob` from
[GitHub](https://github.com/ngreifer/rankprob) with:

``` r
# install.packages("pak")
pak::pak("ngreifer/rankprob")
```

## Example

Below, we use `rankprob` to transform the output of `nnet::multinom()`
to compute the predict probability of two ordered rankings and the
average marginal effect of the predictors on the probability of a given
ranking.

``` r
library(rankprob)

set.seed(123)
n <- 500
data <- data.frame(x1 = rnorm(n),
                   x2 = rnorm(n),
                   x3 = factor(sample(c("t", "c"), n, replace = TRUE)),
                   y = sample(LETTERS[1:4], n, TRUE))

fit <- nnet::multinom(y ~ x1 + x2 + x3, data = data,
                      Hess = TRUE, trace = FALSE)

fit <- convert_rankprob(fit)

p <- predict(fit, rankings = list(c("A", "B", "C", "D"),
                                  c("B", "A", "D", "C")))

head(p)
#>   A > B > C > D B > A > D > C
#> 1    0.04035759    0.03010057
#> 2    0.03961858    0.02972177
#> 3    0.04421595    0.04537628
#> 4    0.04520943    0.04963091
#> 5    0.04064589    0.03816107
#> 6    0.04184766    0.04031138

marginaleffects::avg_slopes(fit, type = "rank_probs",
                            rankings = list(c("A", "B", "C", "D")))
#> 
#>          Group Term          Contrast Estimate Std. Error       z Pr(>|z|)   S    2.5 % 97.5 %
#>  A > B > C > D   x1 mean(dY/dX)       -0.00017    0.00540 -0.0315    0.975 0.0 -0.01076 0.0104
#>  A > B > C > D   x2 mean(dY/dX)        0.00212    0.00523  0.4057    0.685 0.5 -0.00813 0.0124
#>  A > B > C > D   x3 mean(t) - mean(c) -0.00163    0.01062 -0.1536    0.878 0.2 -0.02245 0.0192
#> 
#> Type:  rank_probs 
#> Columns: term, group, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted
```
