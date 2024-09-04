#' 
#' setGeneric(name = "ms2_plot")
#' 
#' #' @title ms2_plot
#' #' @method ms2_plot ms2_data
#' #' @param spectrum1 ms2_data
#' #' @param spectrum2 Spectrum 2.
#' #' @param spectrum1_name Name of spectrum1
#' #' @param spectrum2_name Name of spectrum2
#' #' @param range.mz range.mz
#' #' @param ppm.tol ppm.tol
#' #' @param mz.ppm.thr mz.ppm.thr
#' #' @param xlab xlab.
#' #' @param ylab ylab.
#' #' @param col1 Color 1.
#' #' @param col2 Color 2.
#' #' @param title.size title.size
#' #' @param lab.size lab.size
#' #' @param axis.text.size axis.text.size.
#' #' @param legend.title.size legend.title.size
#' #' @param legend.text.size legend.text.size
#' #' @param interactive_plot Interactive plot or not.
#' #' @export
#' #' @rdname methods-ms2_data
#' #' @return ms2 matching plot
#' 
#' setMethod(f = "ms2_plot",
#'           signature(spectrum1 = "ms2_data"),
#'           function(spectrum1,
#'                    spectrum2,
#'                    spectrum1_name = "spectrum1",
#'                    spectrum2_name = "spectrum2",
#'                    range.mz,
#'                    ppm.tol = 30,
#'                    mz.ppm.thr = 400,
#'                    xlab = "Mass to charge ratio (m/z)",
#'                    ylab = "Relative intensity",
#'                    col1 = "red",
#'                    col2 = "black",
#'                    title.size = 15,
#'                    lab.size = 15,
#'                    axis.text.size = 15,
#'                    legend.title.size = 15,
#'                    legend.text.size = 15,
#'                    interactive_plot = FALSE) {
#'             if (missing(spectrum2)) {
#'               masstools::ms2_plot(spectrum1 = spectrum1@ms2_spectra[[1]])
#'             } else{
#'               masstools::ms2_plot(spectrum1 = spectrum1@ms2_spectra[[1]],
#'                                   spectrum2 = spectrum2@ms2_spectra[[1]])
#'             }
#' 
#'           })
