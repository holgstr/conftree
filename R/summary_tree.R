#' Helper to round numbers according to their magnitude
#'
#' @param x (`numeric`)\cr number to round.
#' @param sig (`number`)\cr non-zero decimal digits to consider for rounding.
#' @return Rounded number.
#' @keywords internal
#'
round_plot <- function(x, sig = 2) {
  if (x < 1 & x != 0) {
    scale <- floor(log10(abs(x)))
    return(round(x, sig - scale - 1))
  }
  return(round(x, 2))
}

#' Helper to get the mean prediction for all inner and outer nodes in a tree
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @return List of the mean prediction in the tree's nodes.
#' @keywords internal
#'
tree_predmean <- function(tree, valid_set) {
  nodes <- partykit::nodeids(tree)
  tree_data <- partykit::data_party(tree)
  lapply(X = nodes, FUN = function(x) {
    # Terminal node ids for each node:
    ids_node_set <- partykit::nodeids(tree, from = x, terminal = TRUE)
    # Corresponding row ids:
    ids_node <- which(tree_data$`(fitted)` %in% ids_node_set)
    # Subset of valid_set:
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    round_plot(get_pred_mean(
      valid_set = valid_set_node
    ), 2)
  })
}

#' Helper to get confident homogeneity for all inner and outer nodes in a tree
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return List of the confident homogeneity in the tree's nodes.
#' @keywords internal
#'
tree_homogeneity <- function(tree, valid_set, alpha, lambda) {
  nodes <- partykit::nodeids(tree)
  tree_data <- partykit::data_party(tree)
  lapply(X = nodes, FUN = function(x) {
    # Terminal node ids for each node:
    ids_node_set <- partykit::nodeids(tree, from = x, terminal = TRUE)
    # Corresponding row ids:
    ids_node <- which(tree_data$`(fitted)` %in% ids_node_set)
    # Subset of valid_set:
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    round_plot(crit_node(
      valid_set = valid_set_node,
      alpha = alpha,
      lambda = lambda
    ), 2)
  })
}

#' Helper to get conformal interval width for all inner and outer nodes in a tree
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return List of the conformal interval width in the tree's nodes.
#' @export
#'
tree_width <- function(tree, valid_set, alpha) {
  nodes <- partykit::nodeids(tree)
  tree_data <- partykit::data_party(tree)
  lapply(X = nodes, FUN = function(x) {
    # Terminal node ids for each node:
    ids_node_set <- partykit::nodeids(tree, from = x, terminal = TRUE)
    # Corresponding row ids:
    ids_node <- which(tree_data$`(fitted)` %in% ids_node_set)
    # Subset of valid_set:
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    round_plot(avg_width(
      valid_set = valid_set_node,
      alpha = alpha
    ), 2)
  })
}

#' Helper to get absolute deviation for all inner and outer nodes in a tree
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @return List of the absolute deviation in the tree's nodes.
#' @keywords internal
#'
tree_dev <- function(tree, valid_set, alpha) {
  nodes <- partykit::nodeids(tree)
  tree_data <- partykit::data_party(tree)
  lapply(X = nodes, FUN = function(x) {
    # Terminal node ids for each node:
    ids_node_set <- partykit::nodeids(tree, from = x, terminal = TRUE)
    # Corresponding row ids:
    ids_node <- which(tree_data$`(fitted)` %in% ids_node_set)
    # Subset of valid_set:
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    round_plot(avg_dev(
      valid_set = valid_set_node,
      alpha = alpha
    ), 2)
  })
}

#' Helper to get the variances across and within a tree's terminal nodes
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param target (`string`)\cr name of the target variable. The target must be a numeric variable.
#' @return Variance across and within the tree's terminal nodes.
#' @keywords internal
#'
tree_vars <- function(tree, target) {
  if (partykit::width(tree) < 2) {NA} else {
    terminals <- partykit::nodeids(tree, terminal = TRUE)
    tree_data <- partykit::data_party(tree)
    means <- lapply(X = terminals, FUN = function(x) {
      ids_node <- which(tree_data$`(fitted)` %in% x)
      mean(tree_data[[target]][ids_node])
    })
    vars <- lapply(X = terminals, FUN = function(x) {
      ids_node <- which(tree_data$`(fitted)` %in% x)
      n_node <- length(ids_node)
      var_node <- stats::var(tree_data[[target]][ids_node]) * (n_node - 1) / n_node
    })
    n <- length(means)
    var_ac <- stats::var(unlist(means)) * (n - 1) / n
    var_in <- mean(unlist(vars))
    c(var_ac, var_in)
  }
}
