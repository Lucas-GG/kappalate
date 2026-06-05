test_that("kappalate synthetic regression runs without S4 extraction failure", {
  set.seed(1)
  n_check <- 500
  check_data <- data.frame(
    Y = 1 + 0.8 * rbinom(n_check, 1, 0.5) + 0.3 * rnorm(n_check) + rnorm(n_check),
    X = rnorm(n_check),
    A = rbinom(n_check, 1, plogis(0.5 * rnorm(n_check))),
    Z = rbinom(n_check, 1, plogis(0.8 * rnorm(n_check)))
  )

  expect_no_error({
    result <- kappalate(Y ~ X | A | Z, check_data, zmodel = "logit", std = "on")
    expect_s3_class(result, "kappalate")
    expect_true(is.matrix(result$coefficients))
    expect_true(is.matrix(result$vcov_matrix))
    expect_equal(ncol(result$coefficients), ncol(result$vcov_matrix))
    expect_equal(ncol(result$vcov_matrix), nrow(result$vcov_matrix))
  })
})
