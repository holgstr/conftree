#' Finding Subgroups in Treatment Effects with Conformal Trees
#'
#' @param data (`data.frame`)\cr data set for model training and uncertainty estimation.
#' @param target (`string`)\cr name of the target variable. The target must be a numeric variable.
#' @param treatment (`string`)\cr name of the treatment variable. If `treatment`\cr is a factor,
#' then the first level is treated as control and the second level as treatment indicator. If `treatment`\cr is a numeric, then
#' zero-one encoding is assumed and `"1"`\cr treated as treatment indicator.
#' @param learner (`model_spec`)\cr the learner for training the prediction model.
#'   See [parsnip::model_spec()] for details.
#' @param cv_folds (`count`)\cr number of CV+ folds.
#' @param alpha (`proportion`)\cr miscoverage rate.
#' @param gamma (`proportion`)\cr regularization parameter ensuring that reduction
#' in the impurity of the confident homogeneity is sufficiently large.
#' @param lambda (`proportion`)\cr balance parameter, quantifying the impact of the average interval length relative
#' to the average absolute deviation (i.e. interval width vs. average absolute deviation)
#' @param max_groups (`count`)\cr maximum number of subgroups.
#' @return The tree.
#' @export
#'
#' @examples
#' library(tidymodels)
#' # Synthetic example data:
#' library(htesim)
#' set.seed(12)
#'dgp <- dgp(p = pF_exp_x1_x2,
#'           m = mF_x1,
#'           t = tF_div_x1_x2,
#'           model = "normal",
#'           xmodel = "unif",
#'           sd = 1)
#'sim <- simulate(object = dgp,
#'                nsim = 500L,
#'                d = 4L)
#' # Initialize learner:
#'linear <- linear_reg() %>%
#'  set_mode("regression") %>%
#'  set_engine("lm")
#' # Detect subgroups:
#' groups <- r2p_hte(
#'   data = sim,
#'   target = "y",
#'   treatment = "trt",
#'   learner = linear,
#'   cv_folds = 500,
#'   alpha = 0.1,
#'   gamma = 0.01,
#'   lambda = 0.5,
#'   max_groups = 8
#' )
#' summary(groups)
r2p_hte <- function(
    data, target, treatment, learner, cv_folds = 1, alpha = 0.1, gamma = 0.01,
    lambda = 0.5, max_groups = 5) {
  # Convert treatment indicator to factor.
  data[[treatment]] <- as.factor(data[[treatment]])
  # Reorder columns to ensure correct column identification for partysplits.
  data <- data[, c(setdiff(names(data), target), target)]
  valid_set <- get_valid_set(data = data, target = target, learner = learner, cv_folds = cv_folds, treatment = treatment)
  # Specific to HTE - treatment itself cannot be used for splitting.
  data <- data[, colnames(data) != treatment, drop = FALSE]
  x_data <- data[, colnames(data) != target, drop = FALSE]
  # Initialize tree.
  node <- partykit::partynode(id = 1)
  tree <- partykit::party(node = node, data = data)
  # Grow tree iteratively.
  while (partykit::width(tree) < max_groups) {
    candidates <- get_candidates(
      tree = tree, x_data = x_data, valid_set = valid_set,
      alpha = alpha, gamma = gamma, lambda = lambda
    )
    # Check if any sensible candidate splits available.
    if (length(candidates) == 0) {
      break
    }
    split <- get_split(candidates = candidates, x_data = x_data)
    # Check if split does not pass gamma-threshold or is negative.
    if (split$gain <= 0) {
      break
    }
    node <- grow_node(node = node, split = split, x_data = x_data)
    tree <- partykit::party(node = node, data = data)
  }
  structure(list(
    tree = tree,
    valid_set = valid_set,
    info = list(
      target = target,
      treatment = treatment,
      cv_folds = cv_folds,
      alpha = alpha,
      gamma = gamma,
      lambda = lambda
    )
  ), class = c("conftree", "r2p_hte"))
}
