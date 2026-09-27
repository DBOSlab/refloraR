test_that("reflora_summary works for full search (herbarium = NULL) or with a vector of herbarium acronyms", {
  skip_if_no_reflora()
  res_ex <- reflora_summary(verbose = FALSE,
                            save = FALSE,
                            dir = "reflora_summary")

  res_ex_some <- reflora_summary(herbarium = c("ALCB"),
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
})


test_that("reflora_summary saves file when save = TRUE", {
  skip_if_no_reflora()
  temp_dir <- tempdir()
  res <- reflora_summary(herbarium = c("RB"),
                         verbose = FALSE,
                         save = TRUE,
                         dir = temp_dir)

  output_path <- file.path(temp_dir, "reflora_summary.csv")
  expect_true(file.exists(output_path))
  unlink(output_path)
})


test_that("reflora_summary fails with invalid herbarium code", {
  skip_if_no_reflora()
  expect_error(
    reflora_summary(herbarium = "FAKE",
                    verbose = FALSE,
                    save = FALSE)
  )
})

test_that("reflora_summary works with trailing slash in dir", {
  skip_if_no_reflora()
  temp_dir <- file.path(tempdir(), "reflora_summary_dir/")
  res <- reflora_summary(herbarium = "RB",
                         verbose = FALSE,
                         save = TRUE,
                         dir = temp_dir)

  expect_s3_class(res, "data.frame")
  expect_true(file.exists(file.path(gsub("/$", "", temp_dir), "reflora_summary.csv")))

  unlink(temp_dir, recursive = TRUE)
})


test_that("reflora_summary prints expected verbose messages", {
  skip_if_no_reflora()
  local_edition(3)  # Required for proper testthat behavior under covr
  expect_message(
    reflora_summary(herbarium = "RB",
                    verbose = TRUE,
                    save = FALSE),
    regexp = "Summarizing specimen collections of RB 1/1"
  )
})


test_that("reflora_summary returns NA if contact email is missing", {
  skip_if_no_reflora()
  df <- reflora_summary(herbarium = "RB",
                        verbose = FALSE,
                        save = FALSE)
  expect_true(is.na(df$hasEmail[1]) || is.character(df$hasEmail[1]))
})


test_that("reflora_summary returns Records column as numeric", {
  skip_if_no_reflora()
  df <- reflora_summary(herbarium = "RB",
                        verbose = FALSE,
                        save = FALSE)
  expect_type(df$Records, "double")
})


test_that("herbarium validation is local and deterministic", {
  expect_null(.validate_herbarium(NULL))
  expect_equal(.validate_herbarium(c(" alcB ", "huefs", "ALCB")), c("ALCB", "HUEFS"))

  expect_error(.validate_herbarium(character()), "non-empty character vector")
  expect_error(.validate_herbarium(c("ALCB", NA_character_)), "non-empty character vector")
  expect_error(.validate_herbarium("ALCB!"), "letters, numbers, or hyphens")
})


test_that(".validate_flag validates verbose/save-style arguments", {
  expect_true(.validate_flag(TRUE, "verbose"))
  expect_false(.validate_flag(FALSE, "save"))

  expect_error(.validate_flag("yes", "verbose"), "must be TRUE or FALSE")
  expect_error(.validate_flag(NA, "save"), "must be TRUE or FALSE")
  expect_error(.validate_flag(c(TRUE, FALSE), "verbose"), "must be TRUE or FALSE")
})


# The tests below mock .get_ipt_info()/.get_herb_info() so reflora_summary()'s
# own orchestration logic (row assembly, sorting, zero-record removal, CSV
# saving, verbose messaging) is covered without any network access.

test_that("reflora_summary() assembles rows from IPT data, sorts them, and drops zero-record collections", {
  fake_info <- list(
    list(character(0), character(0), character(0)),
    c("heph", "zero_herb", "alcb_herbarium"),
    c("HEPH", "ZERO", "ALCB")
  )

  fake_rows <- list(
    heph = list(c("1.223", "2026-09-15 01:08", "28,697"), "Roberta Chacon", "rgchacon@gmail.com",
                "Jardim Botânico de Brasília", "https://ipt.jbrj.gov.br/reflora/resource?r=heph", FALSE),
    zero_herb = list(c("1.0", "2020-01-01 00:00", "0"), "Nobody", NA_character_,
                      "Nowhere", "https://ipt.jbrj.gov.br/reflora/resource?r=zero_herb", FALSE),
    alcb_herbarium = list(c("1.294", "2026-09-15 01:14", "109,437"), "José Marcos", "jose@example.com",
                           "Universidade Federal da Bahia", "https://ipt.jbrj.gov.br/reflora/resource?r=alcb_herbarium", TRUE)
  )

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_rows[[herb_URLs[i]]],
    .package = "refloraR"
  )

  df <- reflora_summary(verbose = FALSE, save = FALSE)

  expect_s3_class(df, "data.frame")
  expect_equal(colnames(df),
               c("collectionCode", "rightsHolder", "Repatriated", "contactPoint",
                 "hasEmail", "Version", "Published.on", "Records", "Reflora_URL"))
  # the zero-record "ZERO" collection is dropped; remaining rows are sorted
  # alphabetically by collectionCode
  expect_equal(df$collectionCode, c("ALCB", "HEPH"))
  expect_equal(df$Records, c(109437, 28697))
  expect_type(df$Records, "double")
  expect_equal(df$Repatriated, c(TRUE, FALSE))
  expect_equal(df$Reflora_URL[df$collectionCode == "HEPH"],
               "https://ipt.jbrj.gov.br/reflora/resource?r=heph")
})

test_that("reflora_summary() saves a CSV file when save = TRUE", {
  fake_info <- list(list(character(0)), c("heph"), c("HEPH"))
  fake_row <- list(c("1.223", "2026-09-15 01:08", "28,697"), "Roberta Chacon", "rgchacon@gmail.com",
                    "Jardim Botânico de Brasília", "https://ipt.jbrj.gov.br/reflora/resource?r=heph", FALSE)

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_row,
    .package = "refloraR"
  )

  tmp_dir <- file.path(tempdir(), "reflora_summary_mock")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  reflora_summary(verbose = FALSE, save = TRUE, dir = tmp_dir)
  expect_true(file.exists(file.path(tmp_dir, "reflora_summary.csv")))
})

test_that("reflora_summary() reports verbose progress messages", {
  fake_info <- list(list(character(0)), c("heph"), c("HEPH"))
  fake_row <- list(c("1.223", "2026-09-15 01:08", "28,697"), "Roberta Chacon", "rgchacon@gmail.com",
                    "Jardim Botânico de Brasília", "https://ipt.jbrj.gov.br/reflora/resource?r=heph", FALSE)

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_row,
    .package = "refloraR"
  )

  expect_message(
    reflora_summary(verbose = TRUE, save = FALSE),
    regexp = "Summarizing specimen collections of HEPH 1/1"
  )
})
