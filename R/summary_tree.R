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
    round(get_pred_mean(
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
    round(crit_node(
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
#' @keywords internal
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
    round(avg_width(
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
    round(avg_dev(
      valid_set = valid_set_node,
      alpha = alpha
    ), 2)
  })
}

#' Helper to get the variance across a tree's terminal nodes
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @return Variance across the tree's terminal nodes.
#' @keywords internal
#'
tree_var_across <- function(tree, valid_set) {
  means <- unlist(tree_predmean(tree = tree, valid_set = valid_set))
  terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
  means <- means[terminal_nodes]
  # We need population variance:
  n <- length(means)
  stats::var(means) * (n - 1) / n
}

#' Helper to get the variance within a tree's terminal nodes
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @return Mean of the variance within the tree's terminal nodes.
#' @keywords internal
#'
tree_var_within <- function(tree, valid_set) {
  mean(unlist(tree_var_nodes(tree = tree,
                      valid_set = valid_set,
                      terminal = TRUE
                      )))
}

#' Helper to get the variances within a tree's nodes
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param terminal (`flag`)\cr if `TRUE` compute only for terminal nodes.
#' @return List of the variance within the tree's nodes.
#' @keywords internal
#'
tree_var_nodes<- function(tree, valid_set, terminal = TRUE) {
  nodes <- partykit::nodeids(obj = tree, terminal = terminal)
  tree_data <- partykit::data_party(party = tree)
  lapply(X = nodes, FUN = function(x) {
    # Terminal node ids for each node:
    ids_node_set <- partykit::nodeids(tree, from = x, terminal = TRUE)
    # Corresponding row ids:
    ids_node <- which(tree_data$`(fitted)` %in% ids_node_set)
    # Subset of valid_set:
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    # We need population variance:
    n <- length(valid_set_node$.pred)
    stats::var(valid_set_node$.pred) * (n - 1) / n
  })
}
