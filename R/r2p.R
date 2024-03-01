#' Robust Recursive Partitioning Using Conformal Prediction
#'
#' @param data (`data.frame`)\cr.
#' @param target (`string`)\cr name of the target variable.
#' @param learner (`model_spec` object)\cr the learner for training the prediction model.
#'   See [parsnip::model_spec()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param cv_folds (`count`)\cr number of CV+ folds.
#' @param gamma (`proportion`)\cr regularization parameter ensuring that reduction
#' in the impurity of the confident homogeneity is sufficiently large.
#' @param lambda (`proportion`)\cr balance parameter, quantifying the impact of the average interval length relative
#' to the average absolute deviation (i.e. interval size vs. within-group homogeneity)
#' @return The tree.
#' @export
#'
#' @examples
#' library(tidymodels)
#' tidymodels_prefer()
#' data(bikes)
#' randforest <- rand_forest(trees = 200, min_n = 5) %>%
#'  set_mode("regression") %>%
#'  set_engine("ranger")
#'  r2ptree(data = bikes, target = "count", learner = randforest,
#'      alpha = 0.05, cv_folds = 1, gamma = 0.01, lambda = 0.5)
r2ptree <- function(data, target, learner, alpha = 0.05, cv_folds = 2, gamma = 0.01, lambda = 0.5) {
  valid_set <- get_valid_set(data = data, target = target, learner = learner, cv_folds = cv_folds)
  x_data <- data[, colnames(data) != target]
  node <- partykit::partynode(id = 1)
  tree <- partykit::party(node = node, data = data)
  while (partykit::width(tree) < 4) {
    terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
    tree_data <- partykit::data_party(tree)
    splits <- lapply(X = terminal_nodes, FUN = function(x) {
      ids_node <- which(x == tree_data[, ncol(tree_data)])
      valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
      process_node(x_data = x_data,
                   node_id = x,
                   valid_set = valid_set_node,
                   alpha = alpha,
                   lambda = lambda)
    })
    splits <- do.call("c", splits)
    conf_crits <- unlist(lapply(splits, FUN = function(x) {x[]$conf_crit}))
    split <- splits[[which.min(conf_crits)]]
    feature_id <- which(names(x_data) == split$feature)
    if (split$feature_type == "numeric") {
      sp <- partykit::partysplit(varid = feature_id,
                                 breaks = split$split_cand)
    } else {
      sp <- partykit::partysplit(varid = feature_id,
                                 index = ifelse(levels(x_data[, feature_id]) %in% split$split_cand, 1L, 2L))
    }
    li <- as.list(node)
    tosplit <- as.integer(split$node_id)
    n <- length(li)
    node_new <- partykit::partynode(id = tosplit, split = sp, kids = list(partykit::partynode(n + 1L), partykit::partynode(n + 2L)))
    li_new <- as.list(node_new)
    li[[tosplit]] <- li_new[[1L]]
    li <- c(li, li_new[-1L])
    node <- partykit::as.partynode(li)
    tree <- partykit::party(node = node, data = data)
  }
  plot(tree)
}
