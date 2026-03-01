test_that("read_mgf parses the bundled MGF file into spectra", {
  mgf_file <- system.file("ms2_data", "QC_MS2_NCE25_1.mgf", package = "massdataset")

  spectra <- suppressWarnings(read_mgf(mgf_file))

  testthat::expect_type(spectra, "list")
  testthat::expect_gt(length(spectra), 0)
  testthat::expect_named(spectra[[1]], c("info", "spec"))
  testthat::expect_equal(names(spectra[[1]]$info), c("mz", "rt"))
  testthat::expect_equal(colnames(spectra[[1]]$spec), c("mz", "intensity"))
  testthat::expect_true(all(is.finite(spectra[[1]]$spec[, "mz"])))
})

test_that("match_mz_rt matches data frames and mass_dataset objects", {
  data1 <- data.frame(mz = c(100, 200), rt = c(30, 60))
  data2 <- data.frame(mz = c(100.0005, 300), rt = c(30.2, 80))

  df_match <- match_mz_rt(data1, data2, mz.tol = 10, rt.tol = 1, rt.error.type = "abs")

  testthat::expect_s3_class(df_match, "data.frame")
  testthat::expect_equal(df_match$Index1[[1]], 1)
  testthat::expect_equal(df_match$Index2[[1]], 1)

  object1 <- make_test_mass_dataset(n_variables = 3, n_samples = 2)
  object2 <- object1
  object2@variable_info$variable_id <- c("other_1", "other_2", "other_3")

  md_match <- match_mz_rt(object1, object2, mz.tol = 10, rt.tol = 30, rt.error.type = "abs")

  testthat::expect_s3_class(md_match, "data.frame")
  testthat::expect_equal(md_match$variable_id1[[1]], "variable1")
  testthat::expect_equal(md_match$variable_id2[[1]], "other_1")
})

test_that("mutate_ms2 attaches matched spectra and process info", {
  object <- make_ms2_test_mass_dataset()
  ms2_dir <- system.file("ms2_data", package = "massdataset")

  mutated <- suppressWarnings(mutate_ms2(
    object = object,
    column = "rp",
    polarity = "positive",
    ms1.ms2.match.mz.tol = 20,
    ms1.ms2.match.rt.tol = 1,
    path = ms2_dir
  ))

  expect_valid_mass_dataset(mutated)
  ms2_data <- extract_ms2_data(mutated)
  testthat::expect_type(ms2_data, "list")
  testthat::expect_length(ms2_data, 1)
  testthat::expect_s4_class(ms2_data[[1]], "ms2_data")
  testthat::expect_true("feature_match" %in% ms2_data[[1]]@variable_id)
  testthat::expect_true("mutate_ms2" %in% names(mutated@process_info))
})
