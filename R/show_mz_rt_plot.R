#' @title mz and rt plot
#' @description mz and rt plot of one mass_dataset class object.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param hex hex or not.
#' @return A ggplot2 object
#' @export
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
#'   show_mz_rt_plot(object)
#'   show_mz_rt_plot(log(object, 10))
#' 
#'   show_mz_rt_plot(object, hex = TRUE)

show_mz_rt_plot <-
  function(object,
           hex = FALSE) {
    check_object_class(object = object, class = "mass_dataset")
    
    variable_info <- object@variable_info
    
    mean_int <-
      object@expression_data %>%
      apply(1, function(x) {
        mean(x, na.rm = TRUE)
      })
    
    variable_info$mean_into <- mean_int
    
    if (hex) {
      plot <-
        variable_info %>% 
        ggplot(aes(rt, mz)) +
        geom_hex() +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) +
        scale_fill_gradient(low = "white", high = "red") +
        labs(x = "Retention time (second)",
             y = "Mass to charge ratio (m/z)")
    } else{
      plot <-
        variable_info %>% 
        ggplot(aes(rt, mz)) +
        geom_point(aes(color = mean_int, size = mean_int)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) +
        scale_color_gradient(low = "white", high = "red") +
        labs(x = "Retention time (second)",
             y = "Mass to charge ratio (m/z)")
    }
    return(plot)
  }
