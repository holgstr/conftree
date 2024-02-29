#' Helper to assess if a numeric split is sensible
#'
#' This checks if the child nodes contain enough observations to result in sensible quantile values.
#' This is to prevent that the alpha-quantile becomes \eqn{\infty} if \eqn{(1 - \alpha)(n + 1) > n}.
#'
#' @param split_cand (`number`)\cr value of the split point.
#' @param covariate (`numeric`)\cr covariate values of the observations in the parent node.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return Logical value indicating if the child nodes contain enough observations.
#' @keywords internal
#'
eval_split_cand_numeric <- function(split_cand, covariate, valid_set, alpha) {
  ids_candidates_left <- which(covariate < split_cand)
  eval_split_cand(ids_candidates_left, valid_set, alpha)
}

#' Helper to assess if a categorical split is sensible
#'
#' This checks if the child nodes contain enough observations to result in sensible quantile values.
#' This is to prevent that the alpha-quantile becomes \eqn{\infty} if \eqn{(1 - \alpha)(n + 1) > n}.
#'
#' @param split_cand (`character`)\cr split point, set of categories representing the left child node.
#' @param covariate (`factor`)\cr covariate values of the observations in the parent node.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return Logical value indicating if the child nodes contain enough observations.
#' @keywords internal
#'
eval_split_cand_categorical <- function(split_cand, covariate, valid_set, alpha) {
  ids_candidates_left <- which(covariate %in% split_cand)
  eval_split_cand(ids_candidates_left, valid_set, alpha)
}

#' Helper to assess if a split is sensible
#'
#' This checks if the child nodes contain enough observations to result in sensible quantile values.
#' This is to prevent that the alpha-quantile becomes \eqn{\infty} if \eqn{(1 - \alpha)(n + 1) > n}.
#'
#' @param ids_left (`integer`)\cr ids of the left child node.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return Logical value indicating if the child nodes contain enough observations.
#' @keywords internal
#'
eval_split_cand <- function(ids_left, valid_set, alpha) {
  ids_candidates_left_valid_set <- ids_left[ids_left %in% valid_set$testing_ids]
  n_left <- length(ids_candidates_left_valid_set)
  n_min <- min(n_left, nrow(valid_set) - n_left)
  alpha >= 1 / (n_min + 1)
}

#' Helper to compute the confident criterion of a numeric split
#'
#' @param node_id (`count`)\cr parent node identifier.
#' @param var_name (`string`)\cr name of the feature to be split.
#' @param split_cand (`number`)\cr value of the split point.
#' @param covariate (`numeric`)\cr covariate values of the observations in the parent node.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return List with elements `node_id`, `feature`, `feature_type`, `split_cand`, `ids_left_child`, `ids_right_child` and `conf_crit`.
#' @keywords internal
#'
process_split_config_numeric <- function(node_id, var_name, split_cand, covariate, valid_set, alpha, lambda) {
  ids_candidates_left <- which(covariate < split_cand)
  ids_candidates_right <- which(covariate >= split_cand)
  conf_crit <- process_split_config(ids_candidates_left, ids_candidates_right, valid_set, alpha, lambda)
  list("node_id" = node_id, "feature" = var_name, "feature_type" = "numeric", "split_cand" = split_cand, "ids_left_child" = ids_candidates_left, "ids_right_child" = ids_candidates_right, "conf_crit" = conf_crit)
}

#' Helper to compute the confident criterion of a categorical split
#'
#' @param node_id (`count`)\cr parent node identifier.
#' @param var_name (`string`)\cr name of the feature to be split.
#' @param split_cand (`character`)\cr split point, set of categories representing the left child node.
#' @param covariate (`factor`)\cr covariate values of the observations in the parent node.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return List with elements `node_id`, `feature`, `feature_type`, `split_cand`, `ids_left_child`, `ids_right_child` and `conf_crit`.
#' @keywords internal
#'
process_split_config_categorical <- function(node_id, var_name, split_cand, covariate, valid_set, alpha, lambda) {
  ids_candidates_left <- which(covariate %in% split_cand)
  ids_candidates_right <- which(!(covariate %in% split_cand))
  conf_crit <- process_split_config(ids_candidates_left, ids_candidates_right, valid_set, alpha, lambda)
  list("node_id" = node_id, "feature" = var_name, "feature_type" = "categorical", "split_cand" = split_cand, "ids_left_child" = ids_candidates_left, "ids_right_child" = ids_candidates_right, "conf_crit" = conf_crit)
}

#' Helper to compute the confident criterion of a split
#'
#' @param ids_left (`integer`)\cr ids of the left child node.
#' @param ids_right (`integer`)\cr ids of the right child node.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return The value of the confident criterion of a split.
#' @keywords internal
#'
process_split_config <- function(ids_left, ids_right, valid_set, alpha, lambda) {
  total_width <- total_width(valid_set, ids_left, ids_right, alpha)
  total_dev <- total_dev(valid_set, ids_left, ids_right, alpha)
  conf_crit(width = total_width, deviation = total_dev, lambda = lambda)
}

#' Helper to find all sensible splits in a covariate
#'
#' @param var_name (`string`)\cr name of the feature to be split.
#' @param x_data (`data.frame`)\cr feature data matrix of the parent node.
#' @param node_id (`count`)\cr parent node identifier.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return List of sensible splits in the covariate.
#' @keywords internal
#'
process_covariate <- function(var_name, x_data, node_id, valid_set, alpha, lambda) {
  covariate = x_data[, colnames(x_data) == var_name]
  ## Numeric covariates:
  if (inherits(covariate, "numeric")) {
    # All split points.
    uniques <- sort(unique(covariate))
    # Vector of split points.
    split_candidates <- (uniques[-length(uniques)] + uniques[-1]) / 2
    split_candidates <- split_candidates[sapply(split_candidates, eval_split_cand_numeric, covariate = covariate, valid_set = valid_set, alpha = alpha)]
    lapply(X = split_candidates, FUN = process_split_config_numeric, node_id = node_id, var_name = var_name, covariate = covariate, valid_set = valid_set, alpha = alpha, lambda = lambda)
  } else {
    # Sort factor levels by mean prediction.
    covariate_with_pred <- covariate[valid_set$testing_ids]
    levels_sorted <- sort(tapply(valid_set$.pred, covariate_with_pred, mean))
    covariate_sorted <- factor(covariate, levels = names(levels_sorted))
    levels_vector <- levels(covariate_sorted)
    # List of left-node factor levels.
    split_candidates <- lapply(seq_along(levels_vector)[-length(levels_vector)], function(i) {
      utils::head(levels_vector, i)
    })
    split_candidates <- split_candidates[sapply(split_candidates, eval_split_cand_categorical, covariate = covariate, valid_set = valid_set, alpha = alpha)]
    lapply(X = split_candidates, FUN = process_split_config_categorical, node_id = node_id, var_name = var_name, covariate = covariate, valid_set = valid_set, alpha = alpha, lambda = lambda)
  }
}

#' Helper to find all sensible splits in a parent node
#'
#' @param x_data (`data.frame`)\cr feature data matrix of the parent node.
#' @param node_id (`count`)\cr parent node identifier.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return List of all sensible splits in the parent node.
#' @keywords internal
#'
process_node <- function(x_data, node_id, valid_set, alpha, lambda) {
  result <- lapply(X = colnames(x_data), FUN = process_covariate, x_data = x_data, node_id = node_id, valid_set = valid_set, alpha = alpha, lambda = lambda)
  do.call("c", result)
}
