% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_parameters.R
\docType{methods}
\name{parse_tidymass_parameter}
\alias{parse_tidymass_parameter}
\title{parse_tidymass_parameter}
\usage{
parse_tidymass_parameter(object)
}
\arguments{
\item{object}{(required) tidymass_parameters class object.}
}
\value{
html report
}
\description{
parse_tidymass_parameter
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
parse_tidymass_parameter(object@process_info$create_mass_dataset)
}
\author{
Xiaotao Shen
\email{shenxt1990@outlook.com}
}
