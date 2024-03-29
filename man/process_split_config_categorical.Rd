% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_node.R
\name{process_split_config_categorical}
\alias{process_split_config_categorical}
\title{Helper to compute the gain in confident homogeneity of a categorical split}
\usage{
process_split_config_categorical(
  node_id,
  var_name,
  split_cand,
  x_data,
  valid_set,
  crit_node,
  alpha,
  gamma,
  lambda
)
}
\arguments{
\item{node_id}{(\code{count})\cr parent node identifier.}

\item{var_name}{(\code{string})\cr name of the feature to be split.}

\item{split_cand}{(\code{character})\cr split point, set of categories representing the left child node.}

\item{x_data}{(\code{data.frame})\cr feature data matrix.}

\item{valid_set}{(\code{data.frame})\cr validation set.
See \code{\link[=get_valid_set]{get_valid_set()}} for details.}

\item{crit_node}{(\code{number})\cr confident homogeneity of the parent node.}

\item{alpha}{(\code{proportion})\cr miscoverage rate.}

\item{gamma}{(\code{proportion})\cr regularization parameter ensuring that reduction
in the impurity of the confident homogeneity is sufficiently large.}

\item{lambda}{(\code{proportion})\cr balance between width and deviation.}
}
\value{
List with elements \code{node_id}, \code{feature}, \code{feature_type}, \code{split_cand},
\code{ids_left_child}, \code{ids_right_child} and \code{gain}.
}
\description{
Helper to compute the gain in confident homogeneity of a categorical split
}
\keyword{internal}
