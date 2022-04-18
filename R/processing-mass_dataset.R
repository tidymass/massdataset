#' @title cbind
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @method cbind mass_dataset
#' @param ... mass_dataset objects
#' @param deparse.level deparse.level
#' @export
#' @rdname processing-mass_dataset
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
#' @param x mass_dataset objects
#' @param y mass_dataset objects
#' @param deparse.level deparse.level
#' @export
#' @rdname processing-mass_dataset
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
#' @param ... mass_datasets
#' @param deparse.level deparse.level
#' @export
#' @rdname processing-mass_dataset
#' @return mass_dataset

rbind.mass_dataset <-
  function(..., deparse.level = 1) {
    xy = list(...)

    object <- NULL
    for(i in seq_along(xy)){
      object <-
        rbind_mass_dataset(object, xy[[i]])
    }
    return(object)
  }



#' @title rbind mass_dataset
#' @param x mass_dataset
#' @param y mass_dataset
#' @param deparse.level deparse.level
#' @export
#' @rdname processing-mass_dataset
#' @return mass_dataset

rbind_mass_dataset <-
  function(x, y, deparse.level = 1) {
    if (is.null(x)) {
      return(y)
    }
    
    if (is.null(y)) {
      return(x)
    }
    
    if (nrow(x@sample_info) != nrow(y@sample_info)) {
      stop("rownames(x) should be same with rownames(y).\n")
    }
    
    if (any(colnames(x) != colnames(y))) {
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
    
    rownames(expression_data_y) =
      purrr::map(rownames(expression_data_y), function(x) {
        if (any(x == rownames(expression_data_x))) {
          paste(x, 2, sep = "_")
        } else{
          x
        }
      }) %>%
      unlist()
    
    variable_info_y$variable_id = rownames(expression_data_y)
    
    expression_data = rbind(expression_data_x, expression_data_y)
    
    sample_info_y =
      sample_info_y %>%
      dplyr::select(-sample_id)
    
    sample_info_note_y =
      sample_info_note_y %>%
      dplyr::filter(!name %in% "sample_id")
    
    colnames(sample_info_y) =
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
    sample_info_note =
      rbind(sample_info_note_x,
            sample_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    ####variable_info
    variable_info =
      variable_info_x %>%
      dplyr::full_join(variable_info_y, by = intersect(colnames(variable_info_x),
                                                       colnames(variable_info_y)))
    
    variable_info_note =
      rbind(variable_info_note_x,
            variable_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    ####annotation_table
    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table =
        annotation_table_x
    }
    
    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table =
        annotation_table_y
    }
    
    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table =
        annotation_table_x
    }
    
    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table =
        rbind(annotation_table_x,
              annotation_table_y) %>%
        dplyr::distinct(.keep_all = TRUE)
    }
    
    variable_info_note =
      rbind(variable_info_note_x,
            variable_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    
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
#' @export
#' @rdname processing-mass_dataset
#' @return mass_dataset class object

head.mass_dataset = function(x, ...) {
  x@expression_data = head(x@expression_data, ...)
  x = update_mass_dataset(x)
  return(x)
}


#' @title tail
#' @method tail mass_dataset
#' @param x x
#' @export
#' @rdname processing-mass_dataset
#' @return mass_dataset class object

tail.mass_dataset = function(x, ...) {
  x@expression_data = tail(x@expression_data, ...)
  x = update_mass_dataset(x)
  return(x)
}
