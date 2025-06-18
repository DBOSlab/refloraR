test_that("reflora_summary works for full search (herbarium = NULL) or with a vector of herbarium acronyms", {
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
  temp_dir <- tempdir()
  res <- reflora_summary(herbarium = c("RB", "HUEFS", "K"),
                         verbose = FALSE,
                         save = TRUE,
                         dir = temp_dir)

  output_path <- file.path(temp_dir, "reflora_summary.csv")
  expect_true(file.exists(output_path))
  unlink(output_path)
})

test_that("reflora_summary fails with invalid herbarium code", {
  expect_error(
    reflora_summary(herbarium = "FAKE",
                    verbose = FALSE,
                    save = FALSE),
    "not found in the REFLORA Virtual Herbarium"
  )
})

test_that("reflora_summary works with trailing slash in dir", {
  temp_dir <- file.path(tempdir(), "reflora_summary_dir/")
  res <- reflora_summary(herbarium = "RB",
                         verbose = FALSE,
                         save = TRUE,
                         dir = temp_dir)

  expect_s3_class(res, "data.frame")
  expect_true(file.exists(file.path(gsub("/$", "", temp_dir), "reflora_summary.csv")))

  unlink(temp_dir, recursive = TRUE)
})

test_that("reflora_summary prints messages in verbose mode", {
  expect_message(
    reflora_summary(herbarium = "RB",
                    verbose = TRUE,
                    save = FALSE),
    "Summarizing specimen collections"
  )
})

test_that("reflora_summary correctly identifies repatriated herbaria", {
  df <- reflora_summary(herbarium = "K",
                        verbose = FALSE,
                        save = FALSE)
  expect_true(df$Repatriated[1])
})

test_that("reflora_summary returns NA if contact email is missing", {
  df <- reflora_summary(herbarium = "RB",
                        verbose = FALSE,
                        save = FALSE)
  expect_true(is.na(df$hasEmail[1]) || is.character(df$hasEmail[1]))
})

test_that("reflora_summary returns Records column as numeric", {
  df <- reflora_summary(herbarium = "RB",
                        verbose = FALSE,
                        save = FALSE)
  expect_type(df$Records, "double")
})
