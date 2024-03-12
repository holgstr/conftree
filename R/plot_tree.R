#' Plot a conftree
#'
#' @param x (`conftree`)\cr tree containing detected subgroups.
#' @param ... additional arguments.
#' @return The plot.
#' @export
#'
#' @examples
#' library(tidymodels)
#' library(ranger)
#' data(bikes)
#' set.seed(1234)
#' randforest <- rand_forest(trees = 200, min_n = 5) %>%
#'   set_mode("regression") %>%
#'   set_engine("ranger")
#' groups <- r2p(
#'   data = bikes,
#'   target = "count",
#'   learner = randforest,
#'   cv_folds = 2,
#'   alpha = 0.05,
#'   gamma = 0.2,
#'   lambda = 0.5
#' )
#' plot(groups)
plot.conftree <- function(x, ...) {
  tree <- x$tree
  valid_set <- x$valid_set
  alpha <- x$info$alpha
  lambda <- x$info$lambda

  # Plot object.
  gg <- ggparty::ggparty(tree,
                         terminal_space = 0.5,
                         add_vars = list(sd = function(data,
                                                       node) {
    list(round(stats::sd(node$data$count), 2))
  })) +
    ggparty::geom_edge() +
    ggparty::geom_edge_label() +
    ggparty::geom_node_splitvar()

  # Additional data: homogeneity in the nodes.
  gg$data$homogeneity <- get_homogeneity(tree,
                                         valid_set,
                                         alpha,
                                         lambda)

  gg <- gg + ggparty::geom_node_label(
    ggplot2::aes(label = paste0(
      "n = ", gg$data$nodesize,
      "\nhomogeneity = ", gg$data$homogeneity,
      "\nid = ", gg$data$id
    )),
    fontface = "bold",
    ids = "all", size = 3, nudge_y = -0.05
  )
  plot(gg)
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
get_homogeneity <- function(tree, valid_set, alpha, lambda) {
  nodes <- partykit::nodeids(tree)
  tree_data <- partykit::data_party(tree)
  lapply(X = nodes, FUN = function(x) {
    # Terminal node ids for each node:
    ids_node_set <- partykit::nodeids(tree, from = x, terminal = TRUE)
    # Corresponding row ids:
    ids_node <- which(tree_data[, ncol(tree_data)] %in% ids_node_set)
    # Subset of valid_set:
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    round(crit_node(valid_set_node, alpha, lambda), 2)
  })
}
