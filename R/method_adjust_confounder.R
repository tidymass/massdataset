#' Adjust Confounders in a mass_dataset Object
#'
#' This function adjusts for confounders in the expression data of a `mass_dataset` object.
#' The function takes a list of confounders from the `sample_info` data and performs a
#' linear regression adjustment on the expression data for each specified confounder.
#'
#' @param object A `mass_dataset` object that contains expression data and sample information.
#' @param confounder_name_list A character vector specifying the names of the confounders
#' to adjust for. These names should be columns in the `sample_info` slot of the `mass_dataset` object.
#'
#' @return A modified `mass_dataset` object with adjusted expression data where confounders
#' have been accounted for.
#'
#' @details
#' This function performs a linear model regression for each feature in the expression data
#' against the specified confounders from the sample information. The residuals of the model
#' are used as the adjusted expression data. If any NA values are present in the confounders
#' or if the confounder names are not found in the `sample_info`, the function will stop
#' and return an error.
#'
#' @section Errors:
#' - The function will stop if `confounder_name_list` is empty.
#' - The function will stop if any of the confounder names in `confounder_name_list`
#'   are not present in the `sample_info`.
#' - The function will stop if any NA values are present in the specified confounders.
#'
#' @examples
#' \dontrun{
#' # Assuming `mass_object` is a valid mass_dataset object and `confounders` is a vector
#' # of column names from the sample_info slot.
#' adjusted_object <- adjust_confounder(object = mass_object, confounder_name_list = confounders)
#' }
#'
#'
#' @export

adjust_confounder <-
  function(object, confounder_name_list) {
    check_object_class(object = object, class = "mass_dataset")
    
    expression_data <-
      extract_expression_data(object = object)
    
    sample_info <-
      extract_sample_info(object = object)
    
    if (length(confounder_name_list) == 0) {
      stop("Please provide the confounder name list.")
    }
    
    if (any(!confounder_name_list %in% colnames(sample_info))) {
      stop("Some confounder names are not in the sample_info.")
    }
    
    ###if any NA in the confounder, stop
    if (any(is.na(sample_info[, confounder_name_list]))) {
      stop("There are NA values in the confounder.")
    }
    
    get_adjsuted_y <- function(x, y) {
      formula_str <- paste("y ~", paste(c(colnames(x)), collapse = " + "))
      
      fit <- lm(as.formula(formula_str), data = data.frame(x, y))
      residuals(fit)
    }
    
    expression_data_new <-
      seq_len(nrow(expression_data)) %>%
      purrr::map(function(i) {
        get_adjsuted_y(x = sample_info[, confounder_name_list], y = as.numeric(expression_data[i, ]))
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    colnames(expression_data_new) <-
      colnames(expression_data)
    
    rownames(expression_data_new) <-
      rownames(expression_data)
    
    object@expression_data <-
      expression_data_new
    
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "adjust_confounder()",
      parameter = list("confounder_name_list" = confounder_name_list),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "adjust_confounder")) {
      process_info$adjust_confounder <- parameter
    } else{
      process_info$adjust_confounder <-
        c(process_info$adjust_confounder, parameter)
    }
    
    object@process_info <- process_info
    return(object)
  }
