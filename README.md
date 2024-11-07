
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rankprob

<!-- badges: start -->
<!-- badges: end -->

The goal of rankprob is to …

## Installation

You can install the development version of rankprob from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ngreifer/rankprob")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rankprob)

fit <- nnet::multinom(y ~ x1 + x2 + x3, data = data,
                      Hess = TRUE)

fit <- convert_rankprob(fit)

p <- predict(fit, rankings = list(c("A", "B", "C", "D"),
                                  c("B", "A", "D", "C")))

head(p)

marginaleffects::avg_slopes(fit, type = "rank_probs")
```
