% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split.R
\docType{methods}
\name{split_mass_dataset}
\alias{split_mass_dataset}
\title{split_mass_dataset}
\usage{
split_mass_dataset(object, by, fun)
}
\arguments{
\item{object}{(required) A mass_dataset class object.}

\item{by}{column name of sample_info or variable_info}

\item{fun}{function used to split by. For example, mz,
should set it as > 100.}
}
\value{
A list of mass_dataset.
}
\description{
Split one mass_dataset into several mass_dataset objects.
}
\examples{
data("expression_data")
data("sample_info")
data("variable_info")

object =
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info,
  )

object <-
activate_mass_dataset(object, what = "sample_info")

new_object <-
  split_mass_dataset(object = object, by = "group")

new_object \%>\% lapply(dim)
new_object \%>\% lapply(colnames)

object <-
  activate_mass_dataset(object, what = "variable_info")

new_object <-
  split_mass_dataset(object = object, by = "rt", fun = function(rt) rt > 600)

new_object \%>\% lapply(dim)
plot(extract_variable_info(new_object[[1]])$rt)
plot(extract_variable_info(new_object[[2]])$rt)
}
\author{
Xiaotao Shen
\email{shenxt1990@outlook.com}
}
