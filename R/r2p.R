#' Robust Recursive Partitioning Using Conformal Prediction
#'
#' @param data (`data.frame`)\cr.
#' @param target (`string`)\cr name of the target variable.
#' @param learner (`model_spec` object)\cr the learner for training the prediction model.
#'   See [parsnip::model_spec()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param cv_folds (`count`)\cr number of CV+ folds.
#' @param gamma (`proportion`)\cr regularization parameter ensuring that reduction
#' in the impurity of the confident homogeneity is sufficiently large.
#' @param lambda (`proportion`)\cr balance parameter, quantifying the impact of the average interval length relative
#' to the average absolute deviation (i.e. interval size vs. within-group homogeneity)
#' @return The tree.
#' @export
#'
#' @examples
#' library(tidymodels)
#' tidymodels_prefer()
#' data(bikes)
#' randforest <- rand_forest(trees = 200, min_n = 5) %>%
#'  set_mode("regression") %>%
#'  set_engine("ranger")
#'  r2p(data = bikes, target = "count", learner = randforest,
#'      alpha = 0.05, cv_folds = 1, gamma = 0.01, lambda = 0.5)
r2p <- function(data, target, learner, alpha = 0.05, cv_folds = 2, gamma = 0.01, lambda = 0.5) {
  # Reorder columns to ensure correct column identification for partysplits.
  data <- data[, c(setdiff(names(data), target), target)]
  valid_set <- get_valid_set(data = data, target = target, learner = learner, cv_folds = cv_folds)
  x_data <- data[, colnames(data) != target]
  # Initialize tree.
  node <- partykit::partynode(id = 1)
  tree <- partykit::party(node = node, data = data)
  # Grow tree iteratively.
  while (partykit::width(tree) < 10) {
    candidates <- get_candidates(tree = tree,
                             x_data = x_data,
                             valid_set = valid_set,
                             alpha = alpha,
                             gamma = gamma,
                             lambda = lambda)
    split <- get_split(candidates = candidates,
                       x_data = x_data)
    # Check if split does not pass gamma-threshold or is negative.
    if (split$gain <= 0) {
      break
    }
    node <- grow_node(node = node,
                      split = split,
                      x_data = x_data)
    tree <- partykit::party(node = node, data = data)
  }
  tree
}
