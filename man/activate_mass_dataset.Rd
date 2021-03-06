% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activate_mass_dataset.R
\docType{methods}
\name{activate_mass_dataset}
\alias{activate_mass_dataset}
\title{activate_mass_dataset}
\usage{
activate_mass_dataset(
  .data,
  what = c("sample_info", "variable_info", "expression_data", "annotation_table")
)
}
\arguments{
\item{.data}{(required) A mass_dataset class object}

\item{what}{(required) What should get be activated?
Possible values are \code{sample_info},
\code{expression_data}, \code{variable_info}, or \code{annotation_table}}
}
\value{
A mass_dataset class object
}
\description{
Determine the context of subsequent manipulations
}
\examples{
data("expression_data")
data("sample_info")
data("variable_info")

object =
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )
object
object@activated
object =
  activate_mass_dataset(.data = object, what = "sample_info")
object@activated
}
\author{
Xiaotao Shen
\email{shenxt1990@outlook.com}
}
