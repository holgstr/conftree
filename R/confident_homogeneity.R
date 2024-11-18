#' Helper to compute the mean prediction of the validation set
#'
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @return the mean of the predicted target variable.
#' @keywords internal
#'
get_pred_mean <- function(valid_set) {
  mean(valid_set$.pred, na.rm = TRUE)
}

#' Helper to compute the conformal alpha-quantile for a vector of residuals
#'
#' @param residuals (`numeric`)\cr residuals of the predictions on the validation set.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return the conformal alpha-quantile.
#' @keywords internal
#'
conf_quantile <- function(residuals, alpha) {
  n <- length(na.omit(residuals))
  assertTRUE(alpha >= 1 / (n + 1))
  prob <- ceiling((1 - alpha) * (n + 1))/n
  stats::quantile(residuals, probs = prob, type = 1, na.rm = TRUE, names = FALSE)
}

#' Helper to compute the mean conformal interval length of a validation set
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return the average conformal interval length.
#' @keywords internal
#'
avg_width <- function(valid_set, alpha) {
  if ("residual_t" %in% names(valid_set)) { # treatment effects:
    # Correct alpha for CATE quantiles:
    alpha_c <- 1 - sqrt(1 - alpha)
    2 * (conf_quantile(valid_set$residual_t, alpha_c) + conf_quantile(valid_set$residual_c, alpha_c))
  } else { # regression:
    2 * conf_quantile(valid_set$residual, alpha)
  }
}

#' Helper to compute the total conformal interval length of a candidate split
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param ids_left (`integer`)\cr ids of the left child node.
#' @param ids_right (`integer`)\cr ids of the right child node.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return the total conformal interval length.
#' @keywords internal
#'
total_width <- function(valid_set, ids_left, ids_right, alpha) {
  valid_set_left <- subset(valid_set, valid_set$testing_ids %in% ids_left)
  valid_set_right <- subset(valid_set, valid_set$testing_ids %in% ids_right)
  # Lee et al. first use sums and then same divisor, so this is mathematically equivalent.
  w_left <- length(valid_set_left) / (length(valid_set_left) + length(valid_set_right))
  w_left * avg_width(valid_set_left, alpha) + (1 - w_left) * avg_width(
    valid_set_right,
    alpha
  )
  #avg_width(valid_set_left, alpha) + avg_width(valid_set_right, alpha)
}

#' Helper to compute the mean absolute deviation of a validation set
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return the average absolute deviation.
#' @keywords internal
#'
avg_dev <- function(valid_set, alpha) {
  group_mean <- get_pred_mean(valid_set)
  conf_width <- avg_width(valid_set, alpha)
  lower_bound <- valid_set$.pred - conf_width / 2
  upper_bound <- valid_set$.pred + conf_width / 2
  distance_to_lower <- abs(group_mean - lower_bound)
  distance_to_upper <- abs(group_mean - upper_bound)
  min_distance <- pmin(distance_to_lower, distance_to_upper)
  min_distance[group_mean >= lower_bound & group_mean <= upper_bound] <- 0
  mean(min_distance)
}

#' Helper to compute the total absolute deviation of a candidate split
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param ids_left (`integer`)\cr ids of the left child node.
#' @param ids_right (`integer`)\cr ids of the right child node.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return the total absolute deviation.
#' @keywords internal
#'
total_dev <- function(valid_set, ids_left, ids_right, alpha) {
  valid_set_left <- subset(valid_set, valid_set$testing_ids %in% ids_left)
  valid_set_right <- subset(valid_set, valid_set$testing_ids %in% ids_right)
  # Lee et al. first use sums and then same divisor, so this is mathematically equivalent.
  w_left <- length(valid_set_left) / (length(valid_set_left) + length(valid_set_right))
  w_left * avg_dev(valid_set_left, alpha) + (1 - w_left) * avg_dev(
    valid_set_right,
    alpha
  )
  #avg_dev(valid_set_left, alpha) + avg_dev(valid_set_right, alpha)
}

#' Helper to compute confident homogeneity
#' @param width (`number`)\cr total conformal interval length of a split.
#' @param deviation (`number`)\cr total absolute deviation of a split.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return the value of the confident homogeneity.
#' @keywords internal
#'
conf_homo <- function(width, deviation, lambda) {
  lambda * width + (1 - lambda) * deviation
}
