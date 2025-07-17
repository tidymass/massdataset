#' @title cbind
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @method cbind mass_dataset
#' @param ... One or more `mass_dataset` objects to be column bound.
#' @param deparse.level Integer controlling the construction of labels in the case of non-matrix-like arguments. Default is 1.
#'
#' @export
#' @return mass_dataset

cbind.mass_dataset <-
  function(..., deparse.level = 1) {
    xy = list(...)
    object <- NULL
    for (i in seq_along(xy)) {
      object <-
        cbind_mass_dataset(object, xy[[i]])
    }
    return(object)
  }

#' @title cbind mass_data class
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x A `mass_dataset` object.
#' @param y Another `mass_dataset` object to be column bound with `x`.
#' @param deparse.level Integer controlling the construction of labels in the case of non-matrix-like arguments. Default is 1.
#' @example
#' library(magrittr)
#'
#' # Create first mass_dataset object
#' expression_data_x <- data.frame(
#'   sample1 = c(1.1, 2.2, 3.3),
#'   sample2 = c(1.5, 2.5, 3.5),
#'   row.names = c("feature1", "feature2", "feature3")
#' )
#'
#' sample_info_x <- data.frame(
#'   sample_id = c("sample1", "sample2"),
#'   batch = c(1, 1),
#'   age = c(30, 32),
#'   class = c("QC", "QC"),
#'   row.names = c("sample1", "sample2")
#' )
#'
#' sample_info_note_x <- data.frame(
#'   name = colnames(sample_info_x),
#'   meaning = c("Sample ID", "Batch ID", "Age", "Sample Class"),
#'   row.names = colnames(sample_info_x)
#' )
#'
#' variable_info <- data.frame(
#'   variable_id = c("feature1", "feature2", "feature3"),
#'   row.names = c("feature1", "feature2", "feature3")
#' )
#'
#' variable_info_note <- data.frame(
#'   name = colnames(variable_info),
#'   meaning = "Feature ID",
#'   row.names = colnames(variable_info)
#' )
#'
#' mass_x <- new(
#'   Class = "mass_dataset",
#'   expression_data = expression_data_x,
#'   sample_info = sample_info_x,
#'   sample_info_note = sample_info_note_x,
#'   variable_info = variable_info,
#'   variable_info_note = variable_info_note,
#'   ms2_data = list(),
#'   annotation_table = data.frame(),
#'   process_info = list(),
#'   other_files = list(),
#'   version = "1.0.0",
#'   activated = "expression_data"
#' )
#'
#' # Create second mass_dataset object
#' expression_data_y <- data.frame(
#'   sample1 = c(1.3, 2.3, 3.4),
#'   sample2 = c(1.7, 2.7, 3.8),
#'   row.names = c("feature1", "feature2", "feature3")
#' )
#'
#' sample_info_y <- data.frame(
#'   sample_id = c("sample1", "sample2"),
#'   batch = c(2, 2),
#'   age = c(31, 34),
#'   class = c("Subject", "Subject"),
#'   row.names = c("sample1", "sample2")
#' )
#'
#' sample_info_note_y <- data.frame(
#'   name = colnames(sample_info_y),
#'   meaning = c("Sample ID", "Batch ID", "Age", "Sample Class"),
#'   row.names = colnames(sample_info_y)
#' )
#'
#' mass_y <- new(
#'   Class = "mass_dataset",
#'   expression_data = expression_data_y,
#'   sample_info = sample_info_y,
#'   sample_info_note = sample_info_note_y,
#'   variable_info = variable_info,
#'   variable_info_note = variable_info_note,
#'   ms2_data = list(),
#'   annotation_table = data.frame(),
#'   process_info = list(),
#'   other_files = list(),
#'   version = "1.0.0",
#'   activated = "expression_data"
#' )
#'
#' # Combine the two objects
#' combined_object <- cbind_mass_dataset(mass_x, mass_y)
#'
#' # View result
#' combined_object@expression_data
#'
#' @export
#' @return mass_dataset

cbind_mass_dataset <-
  function(x, y, deparse.level = 1) {
    if (is.null(x)) {
      return(y)
    }
    
    if (is.null(y)) {
      return(x)
    }
    if (nrow(x@variable_info) != nrow(y@variable_info)) {
      stop("rownames(x) should be same with rownames(y).\n")
    }
    
    if (any(rownames(x) != rownames(y))) {
      stop("rownames(x) should be same with rownames(y).\n")
    }
    
    expression_data_x = x@expression_data
    expression_data_y = y@expression_data
    
    sample_info_x = x@sample_info
    sample_info_y = y@sample_info
    
    sample_info_note_x = x@sample_info_note
    sample_info_note_y = y@sample_info_note
    
    variable_info_x = x@variable_info
    variable_info_y = y@variable_info
    
    variable_info_note_x = x@variable_info_note
    variable_info_note_y = y@variable_info_note
    
    annotation_table_x = x@annotation_table
    annotation_table_y = y@annotation_table
    
    colnames(expression_data_y) =
      purrr::map(colnames(expression_data_y), function(x) {
        if (any(x == colnames(expression_data_x))) {
          paste(x, 2, sep = "_")
        } else{
          x
        }
      }) %>%
      unlist()
    
    sample_info_y$sample_id = colnames(expression_data_y)
    
    expression_data = cbind(expression_data_x, expression_data_y)
    
    sample_info =
      sample_info_x %>%
      dplyr::full_join(sample_info_y,
                       by = intersect(colnames(sample_info_x), colnames(sample_info_y)))
    
    expression_data <-
      expression_data[, sample_info$sample_id, drop = FALSE]
    
    #####sample_info_note
    if (nrow(sample_info_note_x) != 0 |
        nrow(sample_info_note_y) != 0) {
      sample_info_note =
        rbind(sample_info_note_x,
              sample_info_note_y) %>%
        dplyr::distinct(name, .keep_all = TRUE)
    } else{
      sample_info_note = sample_info_note_x
    }
    
    ####variable_info
    variable_info =
      variable_info_x %>%
      dplyr::left_join(variable_info_y, by = intersect(colnames(variable_info_x),
                                                       colnames(variable_info_y)))
    
    if (nrow(variable_info_note_x) != 0 |
        nrow(variable_info_note_y) != 0) {
      variable_info_note =
        rbind(variable_info_note_x,
              variable_info_note_y) %>%
        dplyr::distinct(name, .keep_all = TRUE)
    } else{
      variable_info_note = variable_info_note_x
    }
    
    ####annotation_table
    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table <-
        annotation_table_x
    }
    
    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table <-
        annotation_table_x
    }
    
    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table <-
        annotation_table_y
    }
    
    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table <-
        rbind(annotation_table_x,
              annotation_table_y) %>%
        dplyr::distinct(.keep_all = TRUE)
    }
    
    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = c(x@annotation_table, y@annotation_table),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = c(x@process_info, y@process_info),
      version = as.character(utils::packageVersion(pkg = "massdataset")),
      annotation_table = annotation_table
    )
    
    return(object)
  }


#' @title rbind
#' @method rbind mass_dataset
#' @param ... One or more `mass_dataset` objects to be row bound.
#' @param deparse.level Integer controlling the construction of labels in the case of non-matrix-like arguments. Default is 1.
#'
#' @export
#' @return mass_dataset

rbind.mass_dataset <-
  function(..., deparse.level = 1) {
    xy = list(...)
    
    object <- NULL
    for (i in seq_along(xy)) {
      object <-
        rbind_mass_dataset(object, xy[[i]])
    }
    return(object)
  }



#' Row-wise Binding of mass_dataset Objects
#'
#' This function combines two mass_dataset objects by rows. It checks for
#' compatibility in terms of sample information, variable information, and other
#' attributes before performing the row-wise binding.
#'
#' @param x A mass_dataset object. If missing, the function returns `y`.
#' @param y A mass_dataset object. If missing, the function returns `x`.
#' @param deparse.level Not used, for compatibility only.
#'
#' @return A new mass_dataset object that is the row-wise combination of `x` and `y`.
#'
#' @examples
#' \dontrun{
#' # Assuming mass_dataset1 and mass_dataset2 are two mass_dataset objects
#' result <- rbind_mass_dataset(mass_dataset1, mass_dataset2)
#' }
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @export

rbind_mass_dataset <-
  function(x, y, deparse.level = 1) {
    if (missing(x) & !missing(y)) {
      return(y)
    }
    
    if (missing(y) & !missing(x)) {
      return(x)
    }
    
    if (is.null(x)) {
      return(y)
    }
    
    if (is.null(y)) {
      return(x)
    }
    
    if (nrow(x@sample_info) != nrow(y@sample_info)) {
      stop("colnames(x) should be same with colnames(y).")
    }
    
    if (any(colnames(x) != colnames(y))) {
      stop("colnames(x) should be same with colnames(y).")
    }
    
    if (length(intersect(rownames(x), rownames(y))) > 0) {
      stop("duplicated rownames")
    }
    
    expression_data_x <- x@expression_data
    expression_data_y <- y@expression_data
    
    sample_info_x <- x@sample_info
    sample_info_y <- y@sample_info
    
    sample_info_note_x <- x@sample_info_note
    sample_info_note_y <- y@sample_info_note
    
    variable_info_x <- x@variable_info
    variable_info_y <- y@variable_info
    
    variable_info_note_x <- x@variable_info_note
    variable_info_note_y <- y@variable_info_note
    
    annotation_table_x <- x@annotation_table
    annotation_table_y <- y@annotation_table
    
    # rownames(expression_data_y) <-
    #   purrr::map(rownames(expression_data_y), function(x) {
    #     if (any(x == rownames(expression_data_x))) {
    #       paste(x, 2, sep = "_")
    #     } else{
    #       x
    #     }
    #   }) %>%
    #   unlist()
    #
    # variable_info_y$variable_id <-
    #   rownames(expression_data_y)
    
    expression_data <-
      rbind(expression_data_x, expression_data_y)
    
    sample_info_y <-
      sample_info_y %>%
      dplyr::select(-sample_id)
    
    sample_info_note_y <-
      sample_info_note_y %>%
      dplyr::filter(!name %in% "sample_id")
    
    colnames(sample_info_y) <-
      colnames(sample_info_y) %>%
      purrr::map(function(x) {
        if (x %in% colnames(sample_info_x)) {
          x = paste(x, 2, sep = "_")
          while (x %in% colnames(sample_info_x)) {
            number <-
              stringr::str_extract(x, "_[0-9]{1,3}$") %>%
              stringr::str_replace("_", "") %>%
              as.numeric() %>%
              `+`(1)
            x <- x %>%
              stringr::str_replace("_[0-9]{1,3}$", paste0("_", number))
          }
          x
        } else{
          x
        }
      }) %>%
      unlist()
    
    sample_info_note_y$name = colnames(sample_info_y)
    
    sample_info =
      cbind(sample_info_x,
            sample_info_y)
    
    expression_data <-
      expression_data[, sample_info$sample_id, drop = FALSE]
    
    #####sample_info_note
    sample_info_note <-
      rbind(sample_info_note_x,
            sample_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    ####variable_info
    variable_info <-
      variable_info_x %>%
      dplyr::full_join(variable_info_y, by = intersect(colnames(variable_info_x),
                                                       colnames(variable_info_y)))
    
    variable_info_note <-
      rbind(variable_info_note_x,
            variable_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    ####annotation_table
    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table <-
        annotation_table_x
    }
    
    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table <-
        annotation_table_y
    }
    
    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table <-
        annotation_table_x
    }
    
    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table <-
        rbind(annotation_table_x,
              annotation_table_y) %>%
        dplyr::distinct(.keep_all = TRUE)
    }
    
    variable_info_note <-
      rbind(variable_info_note_x,
            variable_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    ###check sample_info
    duplicated_item <-
      colnames(sample_info) %>%
      stringr::str_replace("_2$", "") %>%
      table() %>%
      `>=`(2) %>%
      which() %>%
      names()
    
    if (length(duplicated_item) > 0) {
      remove_items <- NULL
      for (temp in duplicated_item) {
        temp1 <-
          sample_info %>%
          dplyr::select(!!!dplyr::syms(temp)) %>%
          as.data.frame()
        
        temp2 <-
          sample_info %>%
          dplyr::select(!!!dplyr::syms(paste0(temp, "_2"))) %>%
          as.data.frame()
        
        temp1 <- temp1[, 1] %>%
          as.character()
        temp2 <- temp2[, 1] %>%
          as.character()
        
        temp1[is.na(temp1)] <- ""
        temp2[is.na(temp2)] <- ""
        
        if (all(temp1 == temp2)) {
          remove_items <- c(remove_items, paste0(temp, "_2"))
        }
      }
      
      if (length(remove_items) > 0) {
        sample_info <-
          sample_info %>%
          dplyr::select(!dplyr::one_of(remove_items))
        
        sample_info_note <-
          sample_info_note %>%
          dplyr::filter(!name %in% remove_items)
      }
    }
    
    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = c(x@ms2_data, y@ms2_data),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = c(x@process_info, y@process_info),
      version = as.character(utils::packageVersion(pkg = "massdataset")),
      annotation_table = annotation_table
    )
    
    return(object)
  }

#' @title head
#' @method head mass_dataset
#' @param x x
#' @param ... Additional parameters.
#' @export
#' @return mass_dataset class object

head.mass_dataset = function(x, ...) {
  x@expression_data = head(x@expression_data, ...)
  x = update_mass_dataset(x)
  return(x)
}


#' @title tail
#' @method tail mass_dataset
#' @param x x
#' @param ... Additional parameters.
#' @export
#' @return mass_dataset class object

tail.mass_dataset = function(x, ...) {
  x@expression_data = tail(x@expression_data, ...)
  x = update_mass_dataset(x)
  return(x)
}
