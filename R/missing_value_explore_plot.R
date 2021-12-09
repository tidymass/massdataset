#' @title Add NA number for each feature to variable_info
#' @description Add NA number for each feature to variable_info
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param show_row_names show_row_names. see?ComplexHeatmap::Heatmap
#' @param show_column_names show_column_names see?ComplexHeatmap::Heatmap
#' @param column_names_rot column_names_rot see?ComplexHeatmap::Heatmap
#' @param cell_color cell color
#' @param row_names_side row names side
#' @param ... Other parameters for ComplexHeatmap::Heatmap
#' @return A ggplot2 object
#' @export
#' @examples
#' library(massdataset)
#' library(plyr)
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
#' object
#' 
#' ##show missing values plot
#' show_missing_values(object)
#' 
#' show_missing_values(object[1:10,], cell_color = "white")
#' 
#' ###only show subject samples
#' object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   filter(class == "Subject") %>%
#'   show_missing_values()
#' 
#' ###only show QC samples
#' object %>%
#'   activate_mass_dataset(what = "expression_data") %>%
#'   dplyr::select(contains("QC")) %>%
#'   show_missing_values()
#' 
#' ###only show features with mz < 100
#' object %>%
#'   activate_mass_dataset(what = "variable_info") %>%
#'   dplyr::filter(mz < 100) %>%
#'   show_missing_values(cell_color = "white",
#'                       show_row_names = TRUE,
#'                       row_names_side = "left")

show_missing_values =
  function(object,
           show_row_names = FALSE,
           show_column_names = TRUE,
           column_names_rot,
           cell_color = "transparent",
           row_names_side = "right",
           ...) {
    
    check_object_class(object = object, class = "mass_dataset")
    if(missing(column_names_rot)){
      column_names_rot = 45
    }
    
    expression_data = object@expression_data
    
    expression_data[!is.na(expression_data)] = "2"
    expression_data[is.na(expression_data)] = "1"
    colors = structure(c("1", "2"), names = c("1", "2"))
    plot = 
      suppressMessages(
        ComplexHeatmap::Heatmap(
          as.matrix(expression_data),
          name = "MV",
          cluster_columns = FALSE,
          cluster_rows = FALSE,
          show_row_names = show_row_names,
          show_column_names = show_column_names,
          column_names_rot = 45,
          col = colors,
          rect_gp = gpar(col = cell_color), 
          show_heatmap_legend = FALSE, 
          row_names_side = row_names_side
          
          # ...
        )
      )
    
    plot = ggplotify::as.ggplot(plot)
    
    return(plot)
  }
