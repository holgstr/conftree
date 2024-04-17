#' Helper to retrieve validation set and predictions
#'
#' @param data (`data.frame`)\cr.
#' @param target (`string`)\cr name of the target variable.
#' @param learner (`model_spec` object)\cr the learner for training the prediction model.
#'   See [parsnip::model_spec()] for details.
#' @param cv_folds (`count`)\cr number of CV+ folds.
#' @param treatment (`string`)\cr name of the treatment variable, if applicable.
#' @return Data frame with predictions and indices.
#' @keywords internal
#'
get_valid_set <- function(data, target, learner, cv_folds, treatment = NULL) {
  # Get CV+ splits. As '1' encodes a single train-test split, we create two
  # folds in this case.
  folds <- rsample::vfold_cv(data, v = max(cv_folds, 2), strata = treatment)
  predictions <- lapply(folds$splits, function(split) {
    # Extract the training and testing data for this fold.
    training_data <- rsample::analysis(split)
    testing_data <- rsample::assessment(split)
    testing_ids <- rsample::complement(split)
    # Fit the model on the training data.
    model <- learner %>%
      parsnip::fit(stats::as.formula(paste(target, "~ .")), data = training_data)
    # Predict on the testing data.
    if (is.null(treatment)) {
      cbind(stats::predict(model, testing_data), testing_ids)
    } else {
      treat_levels <- levels(testing_data[[treatment]])
      # Treatment model predictions:
      testing_data[[treatment]] <- treat_levels[1]
      pred_t <- stats::predict(model, testing_data)[[1]]
      # Control model predictions:
      testing_data[[treatment]] <- treat_levels[2]
      pred_c <- stats::predict(model, testing_data)[[1]]
      # Format output:
      data.frame(pred_treat = pred_t, pred_ctrl = pred_c, testing_ids)
    }
  })
  # Special case split conformal prediction if cv_fold = 1.
  if (cv_folds == 1) {
    valid_set <- predictions[[1]]
  } else {
    valid_set <- do.call("rbind", predictions)
  }
  # Compute residuals:
  if (is.null(treatment)) {
    valid_set$residual <- abs(data[[target]][valid_set$testing_ids] - valid_set$.pred)
  } else {
    valid_set$residual_t <- abs(data[[target]][valid_set$testing_ids] - valid_set$pred_treat)
    valid_set$residual_c <- abs(data[[target]][valid_set$testing_ids] - valid_set$pred_ctrl)
    # Compute individual treatment effects:
    valid_set$.pred <- valid_set$pred_treat - valid_set$pred_ctrl
    valid_set <- valid_set[ , !(names(valid_set) %in% c("pred_treat", "pred_ctrl"))]
  }
  valid_set[order(valid_set$testing_ids), ][ , order(names(valid_set))]
}
