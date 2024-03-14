#' Plot a conftree
#'
#' @param x (`conftree`)\cr tree containing detected subgroups.
#' @param ... additional arguments.
#' @return The plot.
#' @importFrom rlang .data
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
#'   lambda = 0.5,
#'   max_groups = 3
#' )
#' plot(groups)
plot.conftree <- function(x, ...) {
  tree <- x$tree

  target <- x$info$target
  valid_set <- x$valid_set
  alpha <- x$info$alpha
  lambda <- x$info$lambda

  # Plot object.
  gg <- ggparty::ggparty(tree,
    terminal_space = 0.5) +
    ggparty::geom_edge() +
    ggparty::geom_edge_label() +
    ggparty::geom_node_splitvar()

  # Add data: mean prediction in the nodes.
  gg$data$predmean <- tree_predmean(
    tree,
    valid_set
  )
  # Add data: homogeneity in the nodes.
  gg$data$homogeneity <- tree_homogeneity(
    tree,
    valid_set,
    alpha,
    lambda
  )
  # Add data: Conformal interval width in the nodes.
  gg$data$width <- tree_width(
    tree,
    valid_set,
    alpha
  )
  # Add data: Conformal interval width in the nodes.
  gg$data$dev <- tree_dev(
    tree,
    valid_set,
    alpha
  )
  # Add data: SD within the outer nodes.
  gg$data$sd <- as.list(round(sqrt(unlist(tree_var_nodes(
    tree,
    valid_set,
    terminal = FALSE
  ))), 2))
  # Add data to plot.
  gg <- gg + ggparty::geom_node_label(
    ggplot2::aes(label = paste0(
      "n = ", gg$data$nodesize,
      "\nmean = ", gg$data$predmean,
      "\nsd = ", gg$data$sd
      # "\nhomogeneity = ", gg$data$homogeneity,
      # "\ninterval width = ", gg$data$width,
      # "\ndeviation = ", gg$data$dev
      # "\nid = ", gg$data$id
    )),
    fontface = "bold",
    ids = "terminal",
    size = 3,
    nudge_y = -0.06
  ) +
    ggparty::geom_node_plot(gglist = list(ggplot2::geom_boxplot(ggplot2::aes(x = "", y = .data[[target]]),
                                                       show.legend = FALSE), xlab("")),
                            height = 0.7,
                            nudge_x = -0.02,
                            nudge_y = -0.13,
                            shared_axis_labels = TRUE)
  plot(gg)
}
