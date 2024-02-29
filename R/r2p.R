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
#'  r2ptree(data = bikes, target = "count", learner = randforest,
#'      alpha = 0.05, cv_folds = 1, gamma = 0.01, lambda = 0.5)
r2ptree <- function(data, target, learner, alpha = 0.05, cv_folds = 2, gamma = 0.01, lambda = 0.5) {
  valid_set <- get_valid_set(data = data, target = target, learner = learner, cv_folds = cv_folds)
  x_data <- data[, colnames(data) != target]
  process_node(x_data, node_id = 1, valid_set, alpha, lambda)
}
