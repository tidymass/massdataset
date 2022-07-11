#' @title intensity_plot
#' @description Get intensity distributation plot for one specific feature.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) A mass_dataset object.
#' @param variable_id Variable id.
#' @param variable_index Variable index. You need to provide this or variable_id.
#' @param color_by which column (in variable_info) is used to color variables
#' @param order_by which column (in variable_info) is used to order variables
#' @param desc Descend the order? TRUE or FALSE.
#' @param interactive Interactive plot or not. TRUE or FALSE.
#' @export
#' @return A ggplot2 class object.
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#' object
#'
#' intensity_plot(object = object, variable_index = 1)
#' intensity_plot(object = object, variable_index = 1, color_by = "class")

intensity_plot <-
  function(object,
           variable_id,
           variable_index,
           color_by,
           order_by,
           desc = FALSE,
           interactive = TRUE) {
    check_object_class(object = object, class = "mass_dataset")
    
    if (missing(variable_id) & missing(variable_index)) {
      stop("provide variable_id or variable_index.\n")
    }
    
    if (!missing(variable_id)) {
      if (!variable_id %in% object@variable_info$variable_id) {
        stop(variable_id, " is not in object.\n")
      }
    } else{
      variable_id = object@variable_info$variable_id[variable_index]
      if (is.na(variable_id)) {
        stop(variable_index, " is out of you variable_info range.\n")
      }
    }
    
    int <-
      as.numeric(object@expression_data[variable_id, ])
    
    temp_data <-
      data.frame(object@sample_info,
                 int,
                 check.names = FALSE)
    
    if (missing(color_by)) {
      color_by = "no"
    } else{
      if (all(colnames(object@sample_info) != color_by)) {
        stop("no ", color_by, " in sample_info, please check.\n")
      }
    }
    
    if (missing(order_by)) {
      order_by = "sample_id"
    } else{
      if (all(colnames(object@sample_info) != order_by) &
          order_by != "na") {
        stop("no ", order_by, " in sample_info, please check.\n")
      }
    }
    
    
    if (desc) {
      temp_data =
        temp_data %>%
        dplyr::arrange(desc(get(order_by))) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
      
    } else{
      temp_data =
        temp_data %>%
        dplyr::arrange(get(order_by)) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
    }
    
    
    plot <-
      temp_data %>%
      ggplot2::ggplot(aes(sample_id, int)) +
      guides(color = guide_legend(title = color_by)) +
      labs(x = "",
           y = "Intensity") +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1
        )
      )
    
    if (color_by == "no") {
      plot <-
        plot +
        ggplot2::geom_point()
    } else{
      plot <-
        plot +
        ggplot2::geom_point(aes(color = get(color_by))) +
        guides(color = guide_legend(title = color_by))
    }
    
    if (interactive) {
      plot <- plotly::ggplotly(plot)
    }
    
    return(plot)
  }
