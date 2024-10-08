% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_value_explore_plot.R
\docType{methods}
\name{show_sample_missing_values}
\alias{show_sample_missing_values}
\title{show_sample_missing_values}
\usage{
show_sample_missing_values(
  object,
  color_by,
  order_by,
  percentage = FALSE,
  show_x_text = TRUE,
  show_x_ticks = TRUE,
  desc = FALSE
)
}
\arguments{
\item{object}{(required) A mass_dataset class object.}

\item{color_by}{which column (in sample_info) is used to color samples}

\item{order_by}{which column (in sample_info) is used to order samples}

\item{percentage}{show MV percentage? TRUE or FALSE.}

\item{show_x_text}{show x axis text or not? TRUE or FALSE}

\item{show_x_ticks}{show x ticks or not? TRUE or FALSE}

\item{desc}{descend sample order or not. TRUE or FALSE.}
}
\value{
A ggplot2 object
}
\description{
show missing values for each sample
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

object

##show missing values plot
show_sample_missing_values(object)
show_sample_missing_values(object, color_by = "class")
show_sample_missing_values(object, color_by = "class", order_by = "na")
show_sample_missing_values(object, color_by = "class", order_by = "na",
                           desc = TRUE)
}
\author{
Xiaotao Shen
\email{shenxt1990@outlook.com}
}
