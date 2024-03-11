
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# **`conftree`**: Regression Trees With Conformal Prediction

This package uses conformal prediction for subgroup detection with
regression trees. It is based on the r2p algorithm, as introduced by
[Lee et. al (NeurIPS,
2020)](https://proceedings.neurips.cc/paper/2020/hash/1819020b02e926785cf3be594d957696-Abstract.html).
In `conftree`, we improve this framework by extending it to
[CV+/Jackknife+](https://arxiv.org/abs/1905.02928). This integrates with
any regression model that can be build with
[`tidymodels`](https://www.tidymodels.org/find/parsnip/).

## Installation

You can install the current development version from GitHub with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("holgstr/conftree")
```

## Quickstart

Let’s find subgroups in the Washington bike share data. We use
`tidymodels` to set a random forest as `learner`, a 5% miscoverage rate
as `alpha`, and 10-fold CV+ to quantify the uncertainty in the resulting
subgroups:

``` r
library(conftree)
library(tidymodels)
data(bikes)

# Specify the learner used for model training:
set.seed(1234)
randforest <- rand_forest(trees = 200, min_n = 5) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# Find optimal subgroups using conformal prediction:
r2p(data = bikes, target = "count", learner = randforest,
    alpha = 0.05, cv_folds = 10, gamma = 0.2, lambda = 0.5)
```

    #> [1] root
    #> |   [2] weekday in Sun: *
    #> |   [3] weekday in Mon, Tue, Wed, Thu, Fri, Sat
    #> |   |   [4] weekday in Sat: *
    #> |   |   [5] weekday in Sun, Mon, Tue, Wed, Thu, Fri
    #> |   |   |   [6] temp <= 6.15: *
    #> |   |   |   [7] temp > 6.15
    #> |   |   |   |   [8] temp <= 28.29
    #> |   |   |   |   |   [9] month <= 2.5: *
    #> |   |   |   |   |   [10] month > 2.5: *
    #> |   |   |   |   [11] temp > 28.29: *
