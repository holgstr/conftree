#' Plot a conftree
#'
#' @param x (`conftree`)\cr tree containing detected subgroups.
#' @param ... additional arguments.
#' @return The plot.
#' @export
#'
plot.conftree <- function(x, ...) {
  tree <- x$tree
  valid_set <- x$valid_set
  alpha <- x$info$alpha
  lambda <- x$info$lambda
  # Additional data: homogeneity in the nodes.
  nodes <- partykit::nodeids(tree)
  tree_data <- partykit::data_party(tree)
  homogeneity <- lapply(X = nodes, FUN = function(x) {
    # Terminal node ids for each node:
    ids_node_set <- partykit::nodeids(tree, from = x, terminal = TRUE)
    # Corresponding row ids:
    ids_node <- which(tree_data[, ncol(tree_data)] %in% ids_node_set)
    # Subset of valid_set:
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    round(crit_node(valid_set_node, alpha, lambda), 2)
  })
  # Plot object.
  gg <- ggparty::ggparty(tree, terminal_space = 0.5, add_vars = list(sd = function(data,
                                                                                   node) {
    list(round(stats::sd(node$data$count), 2))
  })) + ggparty::geom_edge() + ggparty::geom_edge_label() + ggparty::geom_node_splitvar()
  # Add data to object.
  gg$data$homogeneity <- homogeneity
  gg <- gg + ggparty::geom_node_label(
    ggplot2::aes(label = paste0(
      "n = ", gg$data$nodesize,
      "\nhomogeneity = ", homogeneity, "\nid = ", gg$data$id
    )),
    fontface = "bold",
    ids = "all", size = 3, nudge_y = -0.05
  )
  plot(gg)
}
