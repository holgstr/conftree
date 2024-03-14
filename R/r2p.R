#' Finding Subgroups With Regression Trees Using Conformal Prediction
#'
#' @param data (`data.frame`)\cr data set for model training and uncertainty estimation.
#' @param target (`string`)\cr name of the target variable.
#' @param learner (`model_spec`)\cr the learner for training the prediction model.
#'   See [parsnip::model_spec()] for details.
#' @param cv_folds (`count`)\cr number of CV+ folds.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param gamma (`proportion`)\cr regularization parameter ensuring that reduction
#' in the impurity of the confident homogeneity is sufficiently large.
#' @param lambda (`proportion`)\cr balance parameter, quantifying the impact of the average interval length relative
#' to the average absolute deviation (i.e. interval size vs. within-group homogeneity)
#' @param max_groups (`count`)\cr maximum number of subgroups.
#' @return The tree.
#' @export
#'
#' @examples
#' library(tidymodels)
#' library(ranger)
#' data(bikes)
#' set.seed(1234)
#' randforest <- rand_forest(trees = 200, min_n = 5) %>%
#'   set_mode("regression") %>%
#'   set_engine("ranger")
#' groups <- r2p(
#'   data = bikes,
#'   target = "count",
#'   learner = randforest,
#'   cv_folds = 10,
#'   alpha = 0.05,
#'   gamma = 0.2,
#'   lambda = 0.5,
#'   max_groups = 10
#' )
#' groups$tree
r2p <- function(
    data, target, learner, cv_folds = 10, alpha = 0.05, gamma = 0.1,
    lambda = 0.5, max_groups = 10) {
  # Reorder columns to ensure correct column identification for partysplits.
  data <- data[, c(setdiff(names(data), target), target)]
  valid_set <- get_valid_set(data = data, target = target, learner = learner, cv_folds = cv_folds)
  x_data <- data[, colnames(data) != target]
  # Initialize tree.
  node <- partykit::partynode(id = 1)
  tree <- partykit::party(node = node, data = data)
  # Grow tree iteratively.
  while (partykit::width(tree) < max_groups) {
    candidates <- get_candidates(
      tree = tree, x_data = x_data, valid_set = valid_set,
      alpha = alpha, gamma = gamma, lambda = lambda
    )
    # Check if any sensible candidate splits available.
    if (length(candidates) == 0) {
      break
    }
    split <- get_split(candidates = candidates, x_data = x_data)
    # Check if split does not pass gamma-threshold or is negative.
    if (split$gain <= 0) {
      break
    }
    node <- grow_node(node = node, split = split, x_data = x_data)
    tree <- partykit::party(node = node, data = data)
  }
  var_across <- tree_var_across(tree = tree, valid_set = valid_set)
  var_within <- tree_var_within(tree = tree, valid_set = valid_set)
  structure(list(
    tree = tree,
    valid_set = valid_set,
    summmary = list(
      var_across = var_across,
      var_within = var_within
    ),
    info = list(
      target = target,
      cv_folds = cv_folds,
      alpha = alpha,
      gamma = gamma,
      lambda = lambda
    )
  ), class = c("conftree", "r2p"))
}
