msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("massdataset.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  
  theme <- rstudioapi::getThemeInfo()
  
  if (isTRUE(theme$dark))
    crayon::white(x)
  else
    crayon::black(x)
  
}

#' List all packages in the massdataset
#'
#' @param include_self Include massdataset in the list?
#' @export
#' @return massdataset packages
#' @examples
#' massdataset_packages()
massdataset_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("massdataset")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <-
    vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
  
  if (include_self) {
    names <- c(names, "massdataset")
  }
  
  names
}

invert <- function(x) {
  if (length(x) == 0)
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}



#' @title check_column_name
#' @description check_column_name
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param df A data frame.
#' @param column.name new column name
#' @return A column name
#' @export
#' @examples
#' df <-
#'   data.frame(x = 1:2, y = 2:3, y.1 = 3:4)
#' df
#'
#' check_column_name(df = df, column.name = "y")
#' check_column_name(df = df, column.name = "y.1")


check_column_name <-
  function(df, column.name) {
    if (column.name %in% colnames(df)) {
      old_index <-
        grep(column.name, colnames(df), value = TRUE) %>%
        stringr::str_extract("\\.[0-9]{1,2}") %>%
        stringr::str_replace("\\.", "")
      old_index[is.na(old_index)] <- 0
      new_index <- max(as.numeric(old_index)) + 1
      paste(column.name, new_index, sep = ".")
    } else{
      column.name
    }
  }


calculate <-
  function(value,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_freq"),
           ...) {
    switch(
      EXPR = what,
      mean_intensity = mean(value, ...),
      median_intensity = median(value, ...),
      sum_intensity = sum(value, ...)
    )
  }