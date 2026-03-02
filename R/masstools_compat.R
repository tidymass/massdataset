#' Read MGF Files
#'
#' Read MGF files and extract precursor metadata together with fragment peaks.
#'
#' @param file A character vector of MGF file paths.
#'
#' @return A list of spectra. Each element contains `info`, a named numeric
#'   vector with precursor `mz` and `rt`, and `spec`, a numeric matrix with
#'   `mz` and `intensity` columns.
#' @export
read_mgf <- function(file) {
  pbapply::pboptions(style = 1)
  message(crayon::green("Reading mgf data..."))
  ms2 <- purrr::map(
    .x = file,
    .f = function(mgf.data) {
      mgf.data <- list_mgf_records(mgf.data)
      nl.spec <- lapply(mgf.data, function(x) grep("^\\d", x))

      remove_idx <- which(unlist(lapply(nl.spec, length)) == 0)
      if (length(remove_idx) > 0) {
        mgf.data <- mgf.data[-remove_idx]
        nl.spec <- nl.spec[-remove_idx]
      }

      info.mz <- lapply(mgf.data, function(x) grep("^PEPMASS|PRECURSORMZ", x, value = TRUE))
      info.rt <- lapply(mgf.data, function(x) grep("^RTINSECONDS|RETENTIONTIME|RTINMINUTES", x, value = TRUE))

      info.mz <- unlist(info.mz) %>%
        stringr::str_replace("[a-zA-Z|\\:|\\=]{1,20}", "") %>%
        stringr::str_trim()
      info.mz <- unlist(lapply(strsplit(x = info.mz, split = " "), function(x) x[1])) %>%
        as.numeric()
      info.mz <- as.numeric(gsub(pattern = "\\w+=", "", info.mz))

      info.rt <- unlist(info.rt) %>%
        stringr::str_replace("[a-zA-Z|\\:|\\=]{1,20}", "") %>%
        stringr::str_trim() %>%
        as.numeric()
      info.rt <- as.numeric(gsub(pattern = "\\w+=", "", info.rt))

      if (length(mgf.data) == 1) {
        spec <- mapply(function(x, y) {
          temp <- do.call(rbind, strsplit(x[y], split = " "))
          list(temp)
        }, x = mgf.data, y = nl.spec)
      } else {
        spec <- mapply(function(x, y) {
          do.call(rbind, strsplit(x[y], split = " "))
        }, x = mgf.data, y = nl.spec)
      }

      spec <- lapply(spec, function(x) {
        if (ncol(x) == 1 && length(grep("\\\t", x[1, 1])) > 0) {
          x <- as.data.frame(x) %>%
            tidyr::separate(col = 1, sep = "\\\t", into = c("mz", "intensity"))
        }
        temp <- cbind(as.numeric(x[, 1]), as.numeric(x[, 2]))
        temp <- matrix(temp, ncol = 2)
        colnames(temp) <- c("mz", "intensity")
        temp
      })

      mapply(function(x, y, z) {
        info <- c(y, z)
        names(info) <- c("mz", "rt")
        temp <- list(info, as.matrix(x))
        names(temp) <- c("info", "spec")
        list(temp)
      }, x = spec, y = info.mz, z = info.rt)
    }
  )

  spec.info <- ms2[[1]]
  if (length(ms2) > 1) {
    for (i in seq_along(ms2)[-1]) {
      spec.info <- c(spec.info, ms2[[i]])
    }
  }

  remove.idx <- which(unlist(lapply(spec.info, function(x) nrow(x[[2]]))) == 0)
  if (length(remove.idx) != 0) {
    spec.info <- spec.info[-remove.idx]
  }

  spec.info
}

list_mgf_records <- function(file) {
  mgf.data <- readLines(file)
  nl.rec.new <- 1
  idx.rec <- 1
  rec.list <- list()
  for (nl in seq_along(mgf.data)) {
    if (mgf.data[nl] == "END IONS") {
      rec.list[idx.rec] <- list(Compound = mgf.data[nl.rec.new:nl])
      nl.rec.new <- nl + 1
      idx.rec <- idx.rec + 1
    }
  }
  rec.list
}

#' Read mzXML and mzML Files
#'
#' Read mzXML or mzML files.
#'
#' @param file A character vector of file paths.
#' @param threads Integer kept for compatibility with older workflows.
#' @param mode Character string specifying whether files are read with
#'   `"inMemory"` or `"onDisk"`.
#'
#' @return This function always stops in the CRAN build of `massdataset`.
#'   Convert mzXML or mzML files to MGF before calling [mutate_ms2()].
#' @export
read_mzxml <- function(file,
                       threads = 3,
                       mode = c("inMemory", "onDisk")) {
  mode <- match.arg(mode)
  stop(
    "read_mzxml() is not available in the CRAN build of 'massdataset'. ",
    "Convert mzXML or mzML files to MGF before calling mutate_ms2().",
    call. = FALSE
  )
}

#' Match Features by m/z and Retention Time
#'
#' @param data1 First peak table. The first two columns must be `mz` and `rt`.
#' @param data2 Second peak table. The first two columns must be `mz` and `rt`.
#' @param mz.tol Numeric. m/z tolerance in ppm.
#' @param rt.tol Numeric retention time tolerance.
#' @param rt.error.type Character. Use relative or absolute retention time
#'   error.
#' @return A data frame of candidate matches, or `NULL` if no matches are found.
#' @export
#' @rdname match_mz_rt
match_mz_rt.data.frame <- function(data1,
                                   data2,
                                   mz.tol,
                                   rt.tol = 30,
                                   rt.error.type = c("relative", "abs")) {
  rt.error.type <- match.arg(rt.error.type)
  match_mz_rt_default(
    data1 = data1,
    data2 = data2,
    mz.tol = mz.tol,
    rt.tol = rt.tol,
    rt.error.type = rt.error.type
  )
}

#' @export
#' @rdname match_mz_rt
match_mz_rt.matrix <- function(data1,
                               data2,
                               mz.tol,
                               rt.tol = 30,
                               rt.error.type = c("relative", "abs")) {
  rt.error.type <- match.arg(rt.error.type)
  match_mz_rt_default(
    data1 = data1,
    data2 = data2,
    mz.tol = mz.tol,
    rt.tol = rt.tol,
    rt.error.type = rt.error.type
  )
}

match_mz_rt_default <- function(data1,
                                data2,
                                mz.tol,
                                rt.tol = 30,
                                rt.error.type = c("relative", "abs")) {
  rt.error.type <- match.arg(rt.error.type)
  if (nrow(data1) == 0 || nrow(data2) == 0) {
    return(NULL)
  }

  info1 <- apply(data1[, c(1, 2)], 1, list)
  mz2 <- as.numeric(data2[, 1])
  rt2 <- as.numeric(data2[, 2])

  result <- pbapply::pblapply(info1, function(x) {
    temp.mz1 <- x[[1]][[1]]
    temp.rt1 <- x[[1]][[2]]
    mz.error <- abs(temp.mz1 - mz2) * 10^6 / temp.mz1
    if (rt.error.type == "relative") {
      rt.error <- abs(temp.rt1 - rt2) * 100 / temp.rt1
    } else {
      rt.error <- abs(temp.rt1 - rt2)
    }

    j <- which(mz.error <= mz.tol & rt.error <= rt.tol)
    if (length(j) == 0) {
      matrix(NA, ncol = 7)
    } else {
      cbind(j, temp.mz1, mz2[j], mz.error[j], temp.rt1, rt2[j], rt.error[j])
    }
  })

  if (length(result) == 1) {
    result <- cbind(1, result[[1]])
  } else {
    result <- mapply(function(x, y) list(cbind(x, y)), x = seq_along(info1), y = result)
    result <- do.call(rbind, result)
  }

  result <- matrix(
    result[which(!apply(result, 1, function(x) any(is.na(x)))), ],
    ncol = 8
  )
  if (nrow(result) == 0) {
    return(NULL)
  }

  colnames(result) <- c("Index1", "Index2", "mz1", "mz2", "mz error", "rt1", "rt2", "rt error")
  as.data.frame(result)
}

#' @rdname plot_ms2
#' @method plot_ms2 data.frame
#' @export
plot_ms2.data.frame <- function(spectrum1,
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
  if (missing(spectrum1) && missing(spectrum2)) {
    stop("No spectrum1 and spectrum2")
  }

  if (!missing(spectrum1)) {
    spectrum1 <- normalize_spectrum_df(spectrum1)
  }
  if (!missing(spectrum2)) {
    spectrum2 <- normalize_spectrum_df(spectrum2)
  }

  if (!missing(spectrum1) && !missing(spectrum2)) {
    if (missing(range.mz)) {
      range.mz <- c(min(spectrum1[, 1], spectrum2[, 1]), max(spectrum1[, 1], spectrum2[, 1]))
    }

    matched.spec <- ms2_match(
      exp.spectrum = spectrum1,
      lib.spectrum = spectrum2,
      ppm.tol = ppm.tol,
      mz.ppm.thr = mz.ppm.thr
    )
    matched.idx <- which(matched.spec[, "Lib.intensity"] > 0 & matched.spec[, "Exp.intensity"] > 0)

    plot <- ggplot2::ggplot(matched.spec) +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = Exp.mz,
          y = 0,
          xend = Exp.mz,
          yend = Exp.intensity
        ),
        colour = col2
      ) +
      ggplot2::geom_point(
        data = matched.spec[matched.idx, , drop = FALSE],
        mapping = ggplot2::aes(x = Exp.mz, y = Exp.intensity),
        colour = col2
      ) +
      ggplot2::xlim(range.mz[1], range.mz[2]) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::scale_y_continuous(
        limits = c(-1, 1),
        breaks = c(-1, -0.5, 0, 0.5, 1),
        labels = c("1", "0.5", "0", "0.5", "1")
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(color = "black", size = title.size, face = "plain", hjust = 0.5),
        axis.title = ggplot2::element_text(color = "black", size = lab.size, face = "plain"),
        axis.text = ggplot2::element_text(color = "black", size = axis.text.size, face = "plain"),
        legend.title = ggplot2::element_text(color = "black", size = legend.title.size, face = "plain"),
        legend.text = ggplot2::element_text(color = "black", size = legend.text.size, face = "plain")
      ) +
      ggplot2::annotate("text", x = Inf, y = Inf, label = spectrum1_name, color = col2, hjust = 1, vjust = 1) +
      ggplot2::annotate("text", x = Inf, y = -Inf, label = spectrum2_name, color = col1, hjust = 1, vjust = -1) +
      ggplot2::geom_segment(
        data = matched.spec,
        mapping = ggplot2::aes(
          x = Lib.mz,
          y = 0,
          xend = Lib.mz,
          yend = -Lib.intensity
        ),
        colour = col1
      ) +
      ggplot2::geom_point(
        data = matched.spec[matched.idx, , drop = FALSE],
        mapping = ggplot2::aes(x = Lib.mz, y = -Lib.intensity),
        colour = col1
      )

    if (interactive_plot && requireNamespace("plotly", quietly = TRUE)) {
      plot <- plotly::ggplotly(plot)
    }
    return(plot)
  }

  spectrum <- if (!missing(spectrum1)) spectrum1 else spectrum2
  if (missing(range.mz)) {
    range.mz <- c(min(spectrum[, 1]), max(spectrum[, 1]))
  }

  plot <- ggplot2::ggplot(spectrum) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(x = mz, y = 0, xend = mz, yend = intensity),
      colour = col1
    ) +
    ggplot2::xlim(range.mz[1], range.mz[2]) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "black", size = title.size, face = "plain", hjust = 0.5),
      axis.title = ggplot2::element_text(color = "black", size = lab.size, face = "plain"),
      axis.text = ggplot2::element_text(color = "black", size = axis.text.size, face = "plain"),
      legend.title = ggplot2::element_text(color = "black", size = legend.title.size, face = "plain"),
      legend.text = ggplot2::element_text(color = "black", size = legend.text.size, face = "plain")
    )

  if (interactive_plot && requireNamespace("plotly", quietly = TRUE)) {
    plot <- plotly::ggplotly(plot)
  }
  plot
}

#' @rdname plot_ms2
#' @method plot_ms2 matrix
#' @export
plot_ms2.matrix <- function(spectrum1,
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
  if (!missing(spectrum1)) {
    spectrum1 <- as.data.frame(spectrum1, stringsAsFactors = FALSE)
  }
  if (!missing(spectrum2)) {
    spectrum2 <- as.data.frame(spectrum2, stringsAsFactors = FALSE)
  }

  plot_ms2.data.frame(
    spectrum1 = spectrum1,
    spectrum2 = spectrum2,
    spectrum1_name = spectrum1_name,
    spectrum2_name = spectrum2_name,
    range.mz = range.mz,
    ppm.tol = ppm.tol,
    mz.ppm.thr = mz.ppm.thr,
    xlab = xlab,
    ylab = ylab,
    col1 = col1,
    col2 = col2,
    title.size = title.size,
    lab.size = lab.size,
    axis.text.size = axis.text.size,
    legend.title.size = legend.title.size,
    legend.text.size = legend.text.size,
    interactive_plot = interactive_plot
  )
}

normalize_spectrum_df <- function(x) {
  x <- x %>%
    as.data.frame() %>%
    purrr::map(function(value) as.numeric(value)) %>%
    dplyr::bind_cols() %>%
    as.data.frame()
  colnames(x) <- c("mz", "intensity")
  x[, 2] <- x[, 2] / max(x[, 2])
  x
}

ms2_match <- function(exp.spectrum,
                      lib.spectrum,
                      ppm.tol = 30,
                      mz.ppm.thr = 400) {
  exp.spectrum <- as.data.frame(exp.spectrum, stringsAsFactors = FALSE)
  lib.spectrum <- as.data.frame(lib.spectrum, stringsAsFactors = FALSE)
  colnames(exp.spectrum)[1:2] <- c("mz", "intensity")
  colnames(lib.spectrum)[1:2] <- c("mz", "intensity")

  exp.spectrum <- remove_noise(spec = exp.spectrum, ppm.ms2match = ppm.tol, mz.ppm.thr = mz.ppm.thr)
  lib.spectrum <- remove_noise(spec = lib.spectrum, ppm.ms2match = ppm.tol, mz.ppm.thr = mz.ppm.thr)

  match.idx <- lapply(lib.spectrum$mz, function(x) {
    diff.mz <- abs(x - exp.spectrum$mz)
    x[x < mz.ppm.thr] <- mz.ppm.thr
    mz.error <- diff.mz * 10^6 / x
    temp.idx <- which(mz.error < ppm.tol)
    if (length(temp.idx) == 0) {
      return(NA)
    }
    if (length(temp.idx) > 1) {
      return(temp.idx[which.max(exp.spectrum$intensity[temp.idx])])
    }
    temp.idx
  })

  match.idx <- do.call(rbind, match.idx)
  match.idx <- cbind(seq_len(nrow(match.idx)), match.idx)
  colnames(match.idx) <- c("Lib", "Exp")

  non.idx2 <- setdiff(seq_len(nrow(exp.spectrum)), match.idx[, 2][!is.na(match.idx[, 2])])
  if (length(non.idx2) != 0) {
    match.idx2 <- data.frame(NA, non.idx2, stringsAsFactors = FALSE)
    colnames(match.idx2) <- c("Lib", "Exp")
  } else {
    match.idx2 <- NULL
  }

  match.matrix <- as.data.frame(rbind(match.idx, match.idx2), stringsAsFactors = FALSE)
  match.matrix <- data.frame(
    match.matrix,
    lib.spectrum[match.matrix$Lib, c(1, 2)],
    exp.spectrum[match.matrix$Exp, c(1, 2)]
  )
  colnames(match.matrix) <- c("Lib.index", "Exp.index", "Lib.mz", "Lib.intensity", "Exp.mz", "Exp.intensity")
  match.matrix$Lib.intensity[is.na(match.matrix$Lib.intensity)] <- 0
  match.matrix$Exp.intensity[is.na(match.matrix$Exp.intensity)] <- 0
  rownames(match.matrix) <- NULL
  match.matrix
}

remove_noise <- function(spec,
                         ppm.ms2match = 30,
                         mz.ppm.thr = 400) {
  spec <- as.data.frame(spec, stringsAsFactors = FALSE)
  colnames(spec)[1:2] <- c("mz", "intensity")

  if (nrow(spec) == 1) {
    return(spec)
  }

  spec <- spec[order(spec[, 1]), , drop = FALSE]
  mz <- spec[, 1]
  mz <- mz[-1]
  diff.mz <- diff(spec[, 1])
  mz[which(mz < mz.ppm.thr)] <- mz.ppm.thr
  mz.error <- diff.mz * 10^6 / mz
  temp.idx <- which(mz.error < ppm.ms2match)

  if (length(temp.idx) > 0) {
    remove.idx <- lapply(temp.idx, function(idx) c(idx, idx + 1)[which.min(spec[c(idx, idx + 1), 2])])
    remove.idx <- unique(unlist(remove.idx))
    spec <- spec[-remove.idx, , drop = FALSE]
  }

  spec
}
