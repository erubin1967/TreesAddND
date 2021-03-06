% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WND_process_tree.R
\name{WND_process_tree}
\alias{WND_process_tree}
\title{A Cat Function}
\usage{
WND_process_tree(tree_text, class_weights, maximal_allowed_heterogeneity,
  verbose = FALSE)
}
\arguments{
\item{tree_text}{this holds the tree as a vector of text. See package manual for suitable input}

\item{class_weights}{this holds the weight of the different classes. Must be a numerical vector of length 2 of}

\item{maximal_allowed_heterogeneity}{this is the core of this function: if an inner node has less than this value, it is converted to a terminal node. Every termimal node that is not "pure" - that is, the faction of the less common class is above this threshold, it converted to "nd" status, whcih means it cannot be called}

\item{verbose}{should}
}
\description{
This function allows you to express your love of cats.
}
\examples{
WND_process_tree(tree_text,class_weights,maximal_allowed_heterogeneity,verbose)

}
\keyword{decsions_trees}
