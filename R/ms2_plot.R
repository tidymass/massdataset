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
      masstools::ms2_plot(spectrum1 = spectrum1@ms2_spectra[[1]])
    } else{
      masstools::ms2_plot(spectrum1 = spectrum1@ms2_spectra[[1]],
                          spectrum2 = spectrum2@ms2_spectra[[1]])
    }
  }

#' @importFrom masstools ms2_plot
#' @export
masstools::ms2_plot