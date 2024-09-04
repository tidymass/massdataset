#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @export
pivot_longer.mass_dataset <-
  function(data,
           cols,
           names_to = "name",
           names_prefix = NULL,
           names_sep = NULL,
           names_pattern = NULL,
           names_ptypes = NULL,
           names_transform = NULL,
           names_repair = "check_unique",
           values_to = "value",
           values_drop_na = FALSE,
           values_ptypes = NULL,
           values_transform = NULL,
           ...) {
    expression_data <-
      data@expression_data
    
    expression_data <-
      expression_data %>%
      tibble::rownames_to_column(var = "variable_id") %>%
      tidyr::pivot_longer(
        cols = -variable_id,
        names_to = "sample_id",
        values_to = values_to
      ) %>% 
      as.data.frame()
    return(expression_data)
  }

#' @importFrom tidyr pivot_longer
#' @export
tidyr::pivot_longer
