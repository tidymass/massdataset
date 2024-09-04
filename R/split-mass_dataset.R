#' @method split mass_dataset
#' @docType methods
#' @export
split.mass_dataset <- function(x, f, drop = FALSE, ...) {
  activated <-
    slot(object = .data,
         name = "activated")
  
  if (length(activated) == 0) {
    stop("activate you object using activate_mass_dataset() first.\n")
  }
  
  temp_slot <-
    slot(object = .data,
         name = activated)
  if (length(f) == 1) {
    if (f %in% colnames(temp_slot))
      split(temp_slot, f = temp_slot[, f, drop = TRUE], drop = drop, ...)
  } else{
    split(temp_slot, f = f, drop = drop, ...)
  }
  
}


#' Split and Update mass_dataset Object Based on Criteria
#'
#' This function splits a `mass_dataset` object into multiple 
#' subsets based on a specified column in either `sample_info` 
#' or `variable_info`. It also updates the `process_info` slot with 
#' the splitting parameters.
#'
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object A `mass_dataset` object to be split.
#' @param by The column name in `sample_info` or `variable_info` used for splitting.
#' @param fun A function to apply when `by` is numeric. This function should return a logical vector.
#'
#' @return A list of subsetted `mass_dataset` objects.
#' @export
#' @importFrom purrr map
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#'
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'
#' object <-
#' activate_mass_dataset(object, what = "sample_info")
#'
#' new_object <-
#'   split_mass_dataset(object = object, by = "group")
#'
#' new_object %>% lapply(dim)
#' new_object %>% lapply(colnames)
#'
#' object <-
#'   activate_mass_dataset(object, what = "variable_info")
#'
#' new_object <-
#'   split_mass_dataset(object = object, by = "rt", fun = function(rt) rt > 600)
#'
#' new_object %>% lapply(dim)
#' plot(extract_variable_info(new_object[[1]])$rt)
#' plot(extract_variable_info(new_object[[2]])$rt)

split_mass_dataset <-
  function(object,
           by,
           fun) {
    if (missing(by)) {
      stop("by is required.\n")
    }
    
    if (length(object@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    if (!object@activated %in% c("sample_info", "variable_info")) {
      stop("activate should be sample_info or variable_info for split.\n")
    }
    
    x <-
      slot(object = object, name = object@activated)
    
    if (!by %in% colnames(x)) {
      stop("by should be one of columns of ", object@activated)
    }
    
    split_column <-
      x %>%
      dplyr::pull(by)
    
    ##split_column is numeric
    if (is.numeric(split_column)) {
      if (missing(fun)) {
        stop(by, " is numeric, so please provide the fun argument.\n")
      } else{
        result <-
          split_column %>%
          purrr::map(.f = fun) %>%
          unlist()
        
        index <-
          list(which(result), which(!result), which(is.na(result)))
        
        ##remove the length == 0 item
        remain_idx <-
          lapply(index, length) %>%
          unlist() %>%
          `>`(0) %>%
          which()
        index <- index[remain_idx]
      }
    }
    
    ##split_column is character or integer
    if (is.integer(split_column)) {
      split_column <- as.character(split_column)
      if (length(unique(split_column)) > 10) {
        warning("More than 10 groups by ", by, " in ", object@activated, ".")
      }
    }
    
    if (is.character(split_column)) {
      split_column[is.na(split_column)] = "NA"
      index <-
        purrr::map(unique(split_column), function(y) {
          which(split_column == y)
        })
    }
    
    if (length(index) == 1) {
      message("No split. Return the raw mass_dataset.")
      return(list(object))
    }
    
    new_x <- purrr::map(index, function(idx) {
      x[idx, , drop = FALSE]
    })
    
    ###activated is sample_info
    if (object@activated == "sample_info") {
      return_object <-
        purrr::map(new_x, function(temp_sample_info) {
          new_object <- object
          new_object@expression_data <-
            new_object@expression_data[, temp_sample_info$sample_id, drop = FALSE]
          new_object@sample_info <-
            temp_sample_info
          return(new_object)
        })
    }
    
    ###activated is variable_info
    if (object@activated == "variable_info") {
      return_object <-
        purrr::map(new_x, function(temp_variable_info) {
          new_object <- object
          new_object@expression_data <-
            new_object@expression_data[temp_variable_info$variable_id, , drop = FALSE]
          
          new_object@variable_info <-
            temp_variable_info
          
          if (nrow(new_object@annotation_table) > 0) {
            new_object@annotation_table <-
              new_object@annotation_table %>%
              dplyr::filter(variable_id %in% temp_variable_info$variable_id)
          }
          
          if (length(new_object@ms2_data) > 0) {
            new_object@ms2_data <-
              new_object@ms2_data %>%
              purrr::map(function(x) {
                dplyr::filter(x,
                              variable_id %in% new_object@variable_info$variable_id)
              })
          }
          
          if (nrow(new_object@annotation_table) > 0) {
            new_object@annotation_table <-
              new_object@annotation_table %>%
              dplyr::filter(variable_id %in% new_object@variable_info$variable_id)
          }
          
          return(new_object)
        })
    }
    
    ###add parameters
    process_info <- object@process_info
    
    fun_argument <-
      ifelse(
        missing(fun) | is.character(split_column),
        "no",
        paste(stringr::str_trim(deparse(fun)),
              collapse = "")
      )
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "split_mass_dataset",
      parameter = list(by = by,
                       fun = fun_argument),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "split_mass_dataset")) {
      process_info$split_mass_dataset <- parameter
    } else{
      process_info$split_mass_dataset <-
        c(process_info$split_mass_dataset, parameter)
    }
    
    return_object <-
      purrr::map(return_object, function(x) {
        x@process_info <- process_info
        x
      })
    
    if (is.character(split_column)) {
      names(return_object) <-
        unique(split_column)
    }
    
    return(return_object)
  }
