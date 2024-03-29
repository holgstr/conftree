% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_node.R
\name{process_split_config}
\alias{process_split_config}
\title{Helper to compute the gain in confident homogeneity of a split}
\usage{
process_split_config(
  ids_left,
  ids_right,
  valid_set,
  crit_node,
  alpha,
  gamma,
  lambda
)
}
\arguments{
\item{ids_left}{(\code{integer})\cr ids of the left child node.}

\item{ids_right}{(\code{integer})\cr ids of the right child node.}

\item{valid_set}{(\code{data.frame})\cr validation set.
See \code{\link[=get_valid_set]{get_valid_set()}} for details.}

\item{crit_node}{(\code{number})\cr confident homogeneity of the parent node.}

\item{alpha}{(\code{proportion})\cr miscoverage rate.}

\item{gamma}{(\code{proportion})\cr regularization parameter ensuring that reduction
in the impurity of the confident homogeneity is sufficiently large.}

\item{lambda}{(\code{proportion})\cr balance between width and deviation.}
}
\value{
The confident homogeneity of a split.
}
\description{
Helper to compute the gain in confident homogeneity of a split
}
\keyword{internal}
