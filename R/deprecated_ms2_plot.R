#' Plot MS2 Spectra
#'
#' @param spectrum1 An `ms2_data` object.
#' @param spectrum2 An optional second `ms2_data` object for comparison.
#' @param spectrum1_name Label for the first spectrum.
#' @param spectrum2_name Label for the second spectrum.
#' @param range.mz m/z plotting range.
#' @param ppm.tol PPM tolerance for matching peaks.
#' @param mz.ppm.thr m/z threshold used with ppm matching.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param col1 Color for the first spectrum.
#' @param col2 Color for the second spectrum.
#' @param title.size Title text size.
#' @param lab.size Axis label text size.
#' @param axis.text.size Axis tick text size.
#' @param legend.title.size Legend title text size.
#' @param legend.text.size Legend text size.
#' @param interactive_plot Whether to create an interactive plot.
#' @export
ms2_plot <- function(spectrum1,
                     spectrum2,
                     spectrum1_name = "spectrum1",
                     spectrum2_name = "spectrum2",
                     range.mz,
                     ppm.tol = 30,
                     mz.ppm.thr = 400,
                     xlab = "Mass to charge ratio (m/z)",
                     ylab = "Relative intensity",
                     col1 = "red",
                     col2 = "black",
                     title.size = 15,
                     lab.size = 15,
                     axis.text.size = 15,
                     legend.title.size = 15,
                     legend.text.size = 15,
                     interactive_plot = FALSE) {
  UseMethod("ms2_plot")
}

#' @method ms2_plot ms2_data
#' @docType methods
#' @export

ms2_plot.ms2_data <-
  function(spectrum1,
           spectrum2,
           spectrum1_name = "spectrum1",
           spectrum2_name = "spectrum2",
           range.mz,
           ppm.tol = 30,
           mz.ppm.thr = 400,
           xlab = "Mass to charge ratio (m/z)",
           ylab = "Relative intensity",
           col1 = "red",
           col2 = "black",
           title.size = 15,
           lab.size = 15,
           axis.text.size = 15,
           legend.title.size = 15,
           legend.text.size = 15,
           interactive_plot = FALSE) {
    if (missing(spectrum2)) {
      plot_ms2(spectrum1 = spectrum1@ms2_spectra[[1]])
    } else{
      plot_ms2(spectrum1 = spectrum1@ms2_spectra[[1]],
               spectrum2 = spectrum2@ms2_spectra[[1]])
    }
  }
