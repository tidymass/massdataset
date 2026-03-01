test_that("cbind_mass_dataset combines samples with unique sample ids", {
  x <- make_test_mass_dataset()
  y <- x[, 1:2]
  colnames(y@expression_data) <- paste0(colnames(y@expression_data), "_b")
  y@sample_info$sample_id <- colnames(y@expression_data)

  combined <- cbind_mass_dataset(x[, 1:2], y)

  expect_valid_mass_dataset(combined)
  testthat::expect_equal(ncol(combined@expression_data), 4)
  testthat::expect_equal(combined@sample_info$sample_id, colnames(combined@expression_data))
})

test_that("rbind_mass_dataset combines variables with aligned samples", {
  object <- make_test_mass_dataset(n_variables = 6, n_samples = 4)

  combined <- rbind_mass_dataset(object[1:3, ], object[4:6, ])

  expect_valid_mass_dataset(combined)
  testthat::expect_equal(nrow(combined@expression_data), 6)
  testthat::expect_equal(combined@variable_info$variable_id, rownames(combined@expression_data))
})

test_that("merge_mass_dataset returns valid merged objects for multiple join modes", {
  object <- make_test_mass_dataset(n_variables = 6, n_samples = 6)
  x <- object[1:3, 3:5]
  y <- object[2:4, 4:6]

  merged_full <- merge_mass_dataset(
    x = x,
    y = y,
    sample_direction = "full",
    variable_direction = "full"
  )
  merged_inner <- merge_mass_dataset(
    x = x,
    y = y,
    sample_direction = "inner",
    variable_direction = "full"
  )

  expect_valid_mass_dataset(merged_full)
  expect_valid_mass_dataset(merged_inner)
  testthat::expect_gte(ncol(merged_full@expression_data), ncol(merged_inner@expression_data))
  testthat::expect_gte(nrow(merged_full@expression_data), nrow(merged_inner@expression_data))
})

test_that("convert_msdial2mass_dataset builds a valid mass_dataset", {
  data("msdial_table", package = "massdataset")

  object <- convert_msdial2mass_dataset(x = msdial_table)

  expect_valid_mass_dataset(object)
  testthat::expect_true(all(c("sample_id", "class") %in% colnames(object@sample_info)))
  testthat::expect_true(all(c("variable_id", "mz", "rt") %in% colnames(object@variable_info)))
})
