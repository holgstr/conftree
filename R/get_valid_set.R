#' Helper to retrieve validation set and predictions
#'
#' @param data (`data.frame`)\cr.
#' @param target (`string`)\cr name of the target variable.
#' @param learner (`model_spec` object)\cr the learner for training the prediction model.
#'   See [parsnip::model_spec()] for details.
#' @param cv_folds (`count`)\cr number of CV+ folds.
#' @return Data frame with predictions and indices.
#' @keywords internal
#'
get_valid_set <- function(data, target, learner, cv_folds) {
  # Get CV+ splits. As '1' encodes a single train-test split, we create two
  # folds in this case.
  folds <- rsample::vfold_cv(data, v = max(cv_folds, 2))
  predictions <- lapply(folds$splits, function(split) {
    # Extract the training and testing data for this fold
    training_data <- rsample::analysis(split)
    testing_data <- rsample::assessment(split)
    testing_ids <- rsample::complement(split)
    # Fit the model on the training data
    model <- learner %>%
      parsnip::fit(as.formula(paste(target, "~ .")), data = training_data)
    # Predict on the testing data
    cbind(stats::predict(model, testing_data), testing_ids)
  })
  # Special case split conformal prediction if cv_fold = 1.
  if (cv_folds == 1) {
    valid_set <- predictions[[1]]
  } else {
    valid_set <- do.call("rbind", predictions)
  }
  valid_set$residual <- abs(data[[target]][valid_set$testing_ids] - valid_set$.pred)
  valid_set[order(valid_set$testing_ids), ]
}
