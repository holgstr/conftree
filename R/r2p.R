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
  # Get CV+ splits. As "1" encodes a single train-test split, we create two folds in this case.
  folds <- rsample::vfold_cv(data, v = max(cv_folds, 2))
  models <- lapply(folds$splits, function(split) {
    # Extract the training data for this fold
    training_data <- rsample::analysis(split)
    # Fit the model on the training data
    learner %>%
      parsnip::fit(as.formula(paste(target, "~ .")), data = training_data)
  })
}
