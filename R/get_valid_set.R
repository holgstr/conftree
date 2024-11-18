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
    if (is.null(treatment)) { # Regression:
      # Fit the model on the training data.
      model <- learner %>%
        parsnip::fit(stats::as.formula(paste(target, "~ .")), data = training_data)
      # Get predictions:
      cbind(stats::predict(model, testing_data), testing_ids)
    } else { # Treatment Effects:
      treat_levels <- levels(training_data[[treatment]])
      # Treatment model predictions:
      training_treat <- training_data[training_data[[treatment]] == treat_levels[2], ]
      training_treat[[treatment]] <- NULL
      model_treat <- learner %>%
        parsnip::fit(stats::as.formula(paste(target, "~ .")), data = training_treat)
      pred_t <- stats::predict(model_treat, testing_data)[[1]]
      # Control model predictions:
      training_ctrl<- training_data[training_data[[treatment]] == treat_levels[1], ]
      training_ctrl[[treatment]] <- NULL
      model_ctrl <- learner %>%
        parsnip::fit(stats::as.formula(paste(target, "~ .")), data = training_ctrl)
      pred_c <- stats::predict(model_ctrl, testing_data)[[1]]
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
    #### Add treatment-control differentiation:
    valid_set$treatment <- data$treatment[valid_set$testing_ids]
    valid_set$residual_t[valid_set$treatment == levels(valid_set$treatment)[2]] <- NA
    valid_set$residual_c[valid_set$treatment == levels(valid_set$treatment)[1]] <- NA
    valid_set$treatment <- NULL
  }
  valid_set[order(valid_set$testing_ids), ][ , order(names(valid_set))]
}
