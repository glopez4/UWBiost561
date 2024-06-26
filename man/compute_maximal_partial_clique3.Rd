% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_maximal_partial_clique3.R
\name{compute_maximal_partial_clique3}
\alias{compute_maximal_partial_clique3}
\title{compute_maximal_partial_clique3}
\usage{
compute_maximal_partial_clique3(adj_mat, alpha, max_iteration = 1000)
}
\arguments{
\item{adj_mat}{a symmetrical matrix with 0s and 1s. Its diagonal must be 1s and has 5 ~ 50 rows}

\item{alpha}{the prespecified density we can accept for a partial connected clique}

\item{max_iteration}{maximum number of iterations}
}
\value{
the number of nodes that are included in our maximam partial clique
and the actual density of this clique
}
\description{
compute_maximal_partial_clique3
}
