test_that("reflora_summary works for full search (herbarium = NULL) or with a vector of herbarium acronyms", {
  skip_on_cran()
  skip_if_offline()

  res_ex <- reflora_summary(verbose = FALSE,
                            save = FALSE,
                            dir = "reflora_summary")

  res_ex_some <- reflora_summary(herbarium = c("ALCB", "RB", "HUEFS", "US", "K"),
                                 verbose = FALSE,
                                 save = FALSE,
                                 dir = "reflora_summary")

  expect_s3_class(res_ex, "data.frame")
  expect_s3_class(res_ex_some, "data.frame")

  expect_type(res_ex[[3]], "logical")
  expect_type(res_ex[[8]], "double")

  expect_equal(ncol(res_ex), 9)
  expect_gt(nrow(res_ex), 0)

  expect_type(res_ex_some[[3]], "logical")
  expect_type(res_ex_some[[8]], "double")
  expect_equal(ncol(res_ex_some), 9)
  expect_gt(nrow(res_ex_some), 0)

  expect_gt(nrow(res_ex), nrow(res_ex_some))
  expect_gt(sum(res_ex[[8]]), sum(res_ex_some[[8]]))
})

test_that("reflora_summary saves file when save = TRUE", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- tempdir()
  res <- reflora_summary(herbarium = c("RB", "HUEFS", "K"),
                         verbose = FALSE,
                         save = TRUE,
                         dir = temp_dir)

  output_path <- file.path(temp_dir, "reflora_summary.csv")
  expect_true(file.exists(output_path))
  unlink(output_path)
})

test_that("reflora_summary returns error with invalid herbarium code", {
  skip_on_cran()
  skip_if_offline()

  expect_error(
    reflora_summary(herbarium = "INVALIDCODE",
                    verbose = FALSE,
                    save = FALSE)
  )
})
