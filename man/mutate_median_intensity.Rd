% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate_median_intensity.R
\name{mutate_median_intensity}
\alias{mutate_median_intensity}
\title{Add Median Intensity to mass_dataset Object}
\usage{
mutate_median_intensity(object, according_to_samples = "all", na.rm = TRUE)
}
\arguments{
\item{object}{A mass_dataset object.}

\item{according_to_samples}{A character vector specifying the samples to consider for the median calculation. Default is "all".}

\item{na.rm}{Logical, whether to remove NA values before calculating the median. Default is TRUE.}
}
\value{
A modified mass_dataset object with added median intensity information.
}
\description{
This function calculates the median intensity for each variable in the mass_dataset object
based on the specified samples and adds it as a new column to the variable information.
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

##calculate median intensity according to all the samples
object2 =
  mutate_median_intensity(object = object, na.rm = TRUE)

object2

head(extract_variable_info(object))
head(extract_variable_info(object2))

##calculate median intensity according to only QC samples
object3 =
  mutate_median_intensity(object = object2,
                according_to_samples =
              get_sample_id(object)[extract_sample_info(object)$class == "QC"])

object3

head(extract_variable_info(object3))
}
\author{
Xiaotao Shen \href{mailto:shenxt1990@outlook.com}{shenxt1990@outlook.com}
}
