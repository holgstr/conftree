#' Helper to grow the tree by splitting a terminal node
#'
#' @param node (`partynode`)\cr the current tree structure.
#'   See [partykit::partynode()] for details.
#' @param split (`list`)\cr the next split.
#'   See [get_split()] for details.
#' @param x_data (`data.frame`)\cr feature data matrix.
#' @return The updated tree structure.
#' @keywords internal
#'
grow_node <- function(node, split, x_data) {
  feature_id <- which(names(x_data) == split$feature)
  if (split$feature_type == "numeric") {
    sp <- partykit::partysplit(varid = feature_id, breaks = split$split_cand)
  } else {
    sp <- partykit::partysplit(varid = feature_id, index = ifelse(levels(x_data[
      ,
      feature_id
    ]) %in% split$split_cand, 1L, 2L))
  }
  li <- as.list(node)
  tosplit <- as.integer(split$node_id)
  n <- length(li)
  node_new <- partykit::partynode(id = tosplit, split = sp, kids = list(partykit::partynode(n +
    1L), partykit::partynode(n + 2L)))
  li_new <- as.list(node_new)
  li[[tosplit]] <- li_new[[1L]]
  li <- c(li, li_new[-1L])
  partykit::as.partynode(li)
}

#' Helper to get the next split of a tree
#'
#' @param candidates (`list`)\cr all sensible splits in the tree's terminal nodes.
#'   See [get_candidates()] for details.
#' @param x_data (`data.frame`)\cr feature data matrix.
#' @return List specifying the next split
#' @keywords internal
#'
get_split <- function(candidates, x_data) {
  gains <- unlist(lapply(candidates, FUN = function(x) {
    x[]$gain
  }))
  candidates[[which.max(gains)]]
}

#' Helper to get candidates for the next split of a tree
#'
#' @param tree (`party`)\cr tree object.
#'   See [partykit::party()] for details.
#' @param x_data (`data.frame`)\cr feature data matrix.
#' @param valid_set (`data.frame`)\cr validation set.
#'   See [get_valid_set()] for details.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param gamma (`proportion`)\cr regularization parameter ensuring that reduction
#' in the impurity of the confident homogeneity is sufficiently large.
#' @param lambda (`proportion`)\cr balance between width and deviation.
#' @return List of all sensible splits in the tree's terminal nodes.
#' @keywords internal
#'
get_candidates <- function(tree, x_data, valid_set, alpha, gamma, lambda) {
  terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
  tree_data <- partykit::data_party(tree)
  splits <- lapply(X = terminal_nodes, FUN = function(x) {
    ids_node <- which(x == tree_data[, ncol(tree_data)])
    valid_set_node <- valid_set[valid_set$testing_ids %in% ids_node, ]
    process_node(
      x_data = x_data, node_id = x, valid_set = valid_set_node, alpha = alpha,
      gamma = gamma, lambda = lambda
    )
  })
  do.call("c", splits)
}
