make_test_mass_dataset <- function(n_variables = 5, n_samples = 4) {
  expression_data <- matrix(
    seq_len(n_variables * n_samples),
    nrow = n_variables,
    ncol = n_samples
  )
  expression_data <- as.data.frame(expression_data)
  colnames(expression_data) <- paste0("sample", seq_len(n_samples))
  rownames(expression_data) <- paste0("variable", seq_len(n_variables))

  sample_info <- data.frame(
    sample_id = colnames(expression_data),
    injection.order = seq_len(n_samples),
    class = rep(c("QC", "Subject"), length.out = n_samples),
    group = rep(c("case", "control"), length.out = n_samples),
    check.names = FALSE
  )

  variable_info <- data.frame(
    variable_id = rownames(expression_data),
    mz = seq(100, by = 10, length.out = n_variables),
    rt = seq(30, by = 5, length.out = n_variables),
    check.names = FALSE
  )

  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )
}

expect_valid_mass_dataset <- function(object) {
  result <- check_mass_dataset(
    expression_data = object@expression_data,
    sample_info = object@sample_info,
    variable_info = object@variable_info,
    sample_info_note = object@sample_info_note,
    variable_info_note = object@variable_info_note
  )
  testthat::expect_equal(result, "all good.")
}

make_ms2_test_mass_dataset <- function() {
  expression_data <- data.frame(
    sample1 = c(100, 200),
    sample2 = c(110, 210),
    stringsAsFactors = FALSE
  )
  rownames(expression_data) <- c("feature_match", "feature_other")

  sample_info <- data.frame(
    sample_id = c("sample1", "sample2"),
    class = c("QC", "Subject"),
    group = c("case", "control"),
    check.names = FALSE
  )

  variable_info <- data.frame(
    variable_id = c("feature_match", "feature_other"),
    mz = c(75.043762207031, 500.0),
    rt = c(31.147587, 600.0),
    check.names = FALSE
  )

  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )
}
