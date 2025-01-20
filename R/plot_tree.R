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
#'   alpha = 0.1,
#'   gamma = 0.2,
#'   lambda = 0.5,
#'   max_groups = 3
#' )
#' plot(groups)
plot.conftree <- function(x, ...) {
  if (partykit::width(x$tree) < 2) {
    cat("No subgroups detected.")
  } else {
    tree <- x$tree
    target <- x$info$target
    valid_set <- x$valid_set
    alpha <- x$info$alpha
    lambda <- x$info$lambda

    if ("r2p_hte" %in% class(x)) {
      tree$data[[target]] <- NA
      tree$data[[target]][valid_set$testing_ids] <- valid_set$.pred
      colnames(tree$data)[ncol(tree$data)] <- "CATE"
      target <- "CATE"
    }

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
    # Add data: Average absolute deviation in the nodes.
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
        # "\nsd = ", gg$data$sd,
        # "\nhomogeneity = ", gg$data$homogeneity,
        "\ninterval width = ", gg$data$width,
        "\navg. absolute deviation = ", gg$data$dev
        # "\nid = ", gg$data$id
      )),
      fontface = "bold",
      ids = "terminal",
      size = 3,
      nudge_y = -0.06
    ) +
      ggparty::geom_node_plot(gglist = list(ggplot2::geom_boxplot(ggplot2::aes(x = "", y = .data[[target]]),
                                                                  show.legend = FALSE, na.rm = TRUE), ggplot2::xlab(""), ggplot2::theme(
                                                                    axis.title.y = ggplot2::element_text(vjust = 0.7, margin = ggplot2::margin(r = 30))
                                                                  )),
                              height = 0.7,
                              nudge_x = -0.02,
                              nudge_y = -0.16,
                              shared_axis_labels = TRUE)
    plot(gg)
  }
}

#' Print a conftree
#'
#' @param x (`conftree`)\cr tree containing detected subgroups.
#' @param ... additional arguments.
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
#' print(groups)
print.conftree <- function(x, ...) {
  if (partykit::width(x$tree) < 2) {
    cat("No subgroups detected.")
  } else {
    cat("Conformal tree with", partykit::width(x$tree), "subgroups:\n")
    print(x$tree)
    cat("---\n")
    cat("* terminal nodes (subgroups)")
  }
}

#' Prints summary for a conftree
#'
#' @param object (`conftree`)\cr tree containing detected subgroups.
#' @param ... additional arguments.
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
#' summary(groups)
summary.conftree <- function(object, ...) {
  if (partykit::width(object$tree) < 2) {
    cat("No subgroups detected.")
  } else {
    tree <- object$tree
    valid_set <- object$valid_set
    alpha <- object$info$alpha
    group_ids <- partykit::nodeids(tree, terminal = TRUE)
    data <- partykit::data_party(tree)
    ns <- as.numeric(table(data[,ncol(data)]))
    widths <- as.numeric(tree_width(tree, valid_set, alpha))[group_ids]
    devs <- as.numeric(tree_dev(tree, valid_set, alpha))[group_ids]
    means <- as.numeric(tree_predmean(tree, valid_set))[group_ids]
    res <- data.frame("n" = ns, "mean" = means, "width" = widths, "deviation" = devs)
    cat("Conformal tree with", partykit::width(tree), "subgroups:\n")
    print(res)
    cat("---\n")
    cat("Alpha: ", alpha, "Lambda: ", object$info$lambda, "Gamma: ", object$info$gamma, "\n")
  }
}
