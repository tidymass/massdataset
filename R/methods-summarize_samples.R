####merge samples
#' @title merge samples
#' @param object A `mass_dataset` object.
#' @param what Summary statistic used to combine samples within each group.
#' @param group_by Character vector giving one or more columns in `sample_info`
#' used to define the grouping structure.
#' @param ... Additional arguments passed to downstream methods.
#' @return A `mass_dataset` object with grouped samples collapsed into summary
#' profiles and updated `sample_info`.
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#' object <- create_mass_dataset(
#'   expression_data = expression_data,
#'   sample_info = sample_info,
#'   variable_info = variable_info
#' )
#'
#' object2 <- summarise_samples(
#'   object,
#'   what = "mean_intensity",
#'   group_by = "class"
#' )
#' head(extract_expression_data(object2))
#' head(extract_sample_info(object2))
#' @export
summarise_samples <-
  function(object,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity"),
           group_by,
           ...) {
    UseMethod("summarise_samples")
  }

#' @rdname summarise_samples
#' @export
summarize_samples <- summarise_samples

#' @method summarise_samples mass_dataset
#' @rdname summarise_samples
#' @importFrom tibble column_to_rownames
#' @export

summarise_samples.mass_dataset <-
  function(object,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           group_by,
           ...) {
    if (missing(group_by)) {
      return(object)
    }
    
    variable_info <-
      extract_variable_info(object)
    
    sample_info <-
      extract_sample_info(object)
    
    expression_data <-
      extract_expression_data(object)
    
    if (!any(group_by %in% colnames(sample_info))) {
      stop(paste(group_by, collapse = ", "),
           " must be in the sample_info.")
    }
    
    what <-
      match.arg(what)
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    # sample_id2 <-
    #   sample_info %>%
    #   dplyr::pull(group_by) %>%
    #   as.character()
    #
    sample_id2 <-
      sample_info %>%
      dplyr::select(group_by)
    
    if (sum(is.na(sample_id2)) > 0) {
      stop(paste(group_by, collapse = ", "), " contains NA.")
    }
    
    sample_id2 <-
      sample_id2 %>%
      apply(1, function(x) {
        paste(x, collapse = "_")
      })
    
    expression_data2 <-
      unique(sample_id2) %>%
      purrr::map(function(x) {
        temp <-
          expression_data[, which(sample_id2 == x), drop = FALSE] %>%
          apply(1, function(y) {
            calculate(y, what = what)
          }) %>%
          data.frame(x = .)
        colnames(temp) <- x
        temp
      }) %>%
      dplyr::bind_cols() %>%
      as.data.frame()
    
    sample_info2 <-
      sample_info
    
    sample_info2$sample_id <- sample_id2
    
    sample_info2 <-
      sample_info2 %>%
      dplyr::distinct(sample_id, .keep_all = TRUE) %>%
      as.data.frame()
    
    process_info <-
      slot(object, name = "process_info")
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "summarise_samples()",
      parameter = list("what" = what,
                       "group_by" = group_by),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "summarise_samples")) {
      process_info$summarise_samples <- parameter
    } else{
      process_info$summarise_samples <-
        c(process_info$summarise_samples,
          parameter)
    }
    slot(object, "process_info") <- process_info
    slot(object, "sample_info") <- sample_info2
    slot(object, "expression_data") <- expression_data2
    return(object)
  }
