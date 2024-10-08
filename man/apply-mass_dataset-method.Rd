% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary-mass_dataset.R
\name{apply,mass_dataset-method}
\alias{apply,mass_dataset-method}
\title{apply}
\usage{
\S4method{apply}{mass_dataset}(X, MARGIN, FUN, ..., simplify = TRUE)
}
\arguments{
\item{X}{A mass_dataset object.}

\item{MARGIN}{An integer vector indicating which margins should be "swept out".
1 indicates rows, 2 indicates columns.}

\item{FUN}{The function to be applied. This function should take a data vector
argument and return a result of length one.}

\item{...}{Additional arguments to FUN.}

\item{simplify}{Logical; should the result be simplified to
a vector or matrix if possible?}
}
\value{
The result of applying FUN to the margins of X. If simplify is TRUE,
then the result may be a vector or matrix.
}
\description{
apply
}
