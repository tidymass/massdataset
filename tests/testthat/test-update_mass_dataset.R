test_that("update_mass_dataset synchronizes expression_data, sample_info and variable_info", {
  object <- make_test_mass_dataset()

  object@sample_info <- object@sample_info[1:3, , drop = FALSE]
  object@variable_info <- object@variable_info[1:4, , drop = FALSE]

  updated <- update_mass_dataset(object)

  expect_valid_mass_dataset(updated)
  testthat::expect_equal(dim(updated@expression_data), c(4, 3))
  testthat::expect_equal(updated@sample_info$sample_id, colnames(updated@expression_data))
  testthat::expect_equal(updated@variable_info$variable_id, rownames(updated@expression_data))
  testthat::expect_true("update_mass_dataset" %in% names(updated@process_info))
})

test_that("update_variable_info adds missing note rows and reorders columns", {
  object <- make_test_mass_dataset()

  object@variable_info$score <- seq_len(nrow(object@variable_info))
  object@variable_info <- object@variable_info[, c("score", "variable_id", "mz", "rt"), drop = FALSE]

  updated <- update_variable_info(object)

  expect_valid_mass_dataset(updated)
  testthat::expect_equal(updated@variable_info_note$name, colnames(updated@variable_info))
  testthat::expect_true("score" %in% updated@variable_info_note$name)
})

test_that("update_sample_info adds missing note rows and reorders columns", {
  object <- make_test_mass_dataset()

  object@sample_info$batch <- rep(c("A", "B"), length.out = nrow(object@sample_info))
  object@sample_info <- object@sample_info[, c("batch", "sample_id", "injection.order", "class", "group"), drop = FALSE]

  updated <- update_sample_info(object)

  expect_valid_mass_dataset(updated)
  testthat::expect_equal(updated@sample_info_note$name, colnames(updated@sample_info))
  testthat::expect_true("batch" %in% updated@sample_info_note$name)
})
