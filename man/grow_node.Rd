% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grow_tree.R
\name{grow_node}
\alias{grow_node}
\title{Helper to grow the tree by splitting a terminal node}
\usage{
grow_node(node, split, x_data)
}
\arguments{
\item{node}{(\code{partynode})\cr the current tree structure.
See \code{\link[partykit:partynode]{partykit::partynode()}} for details.}

\item{split}{(\code{list})\cr the next split.
See \code{\link[=get_split]{get_split()}} for details.}

\item{x_data}{(\code{data.frame})\cr feature data matrix.}
}
\value{
The updated tree structure.
}
\description{
Helper to grow the tree by splitting a terminal node
}
\keyword{internal}
