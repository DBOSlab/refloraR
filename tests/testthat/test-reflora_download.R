test_that("reflora_download downloads multiple herbaria correctly", {
  skip_if_no_reflora()
  temp_dir <- file.path(tempdir(), "reflora_download_test")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  reflora_download(herbarium = c("ALCB", "HUEFS"),
                   verbose = FALSE,
                   dir = temp_dir)

  # Check: directory created and contains 2 subfolders
  folders <- list.files(temp_dir)
  expect_equal(length(folders), 2)

  # Check: each folder has ≥ 3 files
  contents <- lapply(file.path(temp_dir, folders), list.files)
  expect_true(all(lengths(contents) >= 3))

  unlink(temp_dir, recursive = TRUE)
})


test_that("reflora_download creates _Reflora.csv per herbarium", {
  skip_if_no_reflora()
  temp_dir <- file.path(tempdir(), "reflora_csv_test")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  reflora_download(herbarium = c("ALCB", "HUEFS"),
                   verbose = FALSE,
                   dir = temp_dir)

  downloaded_dirs <- list.files(temp_dir, full.names = TRUE)
  all_csvs <- unlist(lapply(downloaded_dirs, function(x) list.files(x, pattern = "_Reflora.csv", full.names = TRUE)))

  expect_equal(length(all_csvs), 2)
  expect_true(all(file.exists(all_csvs)))

  unlink(temp_dir, recursive = TRUE)
})


test_that("reflora_download returns silently with existing dwca folder", {
  skip_if_no_reflora()
  temp_dir <- file.path(tempdir(), "reflora_download_cached")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)

  reflora_download(herbarium = "ALCB",
                   verbose = FALSE,
                   dir = temp_dir)

  expect_silent(reflora_download(herbarium = "ALCB",
                                 verbose = FALSE,
                                 dir = temp_dir))

  unlink(temp_dir, recursive = TRUE)
})


test_that("reflora_download throws error for invalid herbarium code", {
  skip_if_no_reflora()
  expect_error(
    reflora_summary(herbarium = "INVALIDCODE",
                    verbose = FALSE,
                    save = FALSE)
  )
})


test_that("reflora_download creates directory if it doesn't exist", {
  skip_if_no_reflora()
  tmp_dir <- file.path(tempdir(), "reflora_auto_dir")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  expect_false(dir.exists(tmp_dir))
  reflora_download(herbarium = "ALCB", verbose = FALSE, dir = tmp_dir)
  expect_true(dir.exists(tmp_dir))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("reflora_download prints messages when verbose = TRUE", {
  skip_if_no_reflora()
  tmp_dir <- file.path(tempdir(), "reflora_verbose_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  expect_message(reflora_download(herbarium = "ALCB",
                                  verbose = TRUE,
                                  dir = tmp_dir),
                 "Downloading DwC-A files")

  unlink(tmp_dir, recursive = TRUE)
})


test_that("reflora_download defaults to repatriated = TRUE", {
  skip_if_no_reflora()
  tmp_dir <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  reflora_download(herbarium = c("ALCB", "K"),
                   repatriated = TRUE,
                   verbose = FALSE,
                   dir = tmp_dir)

  expect_true(any(grepl("dwca_k_reflora", list.files(tmp_dir))))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("reflora_download with repatriated = FALSE excludes repatriated herbaria", {
  skip_if_no_reflora()
  tmp_dir <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  reflora_download(herbarium = c("ALCB", "K"),
                   repatriated = FALSE,
                   verbose = FALSE,
                   dir = tmp_dir)

  expect_false(any(grepl("dwca_k_reflora", list.files(tmp_dir))))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("reflora_download prints messages with verbose = TRUE", {
  skip_if_no_reflora()
  tmp_dir <- file.path(tempdir(), "reflora_verbose_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
  expect_message(reflora_download(herbarium = "ALCB",
                                  verbose = TRUE))
  unlink(tmp_dir, recursive = TRUE)
})


# The tests below mock .get_ipt_info()/.get_herb_info() so reflora_download()'s
# own orchestration logic (directory creation, per-collection row assembly,
# zero-record skipping, repatriated-collection skipping, already-downloaded
# detection) is covered without any network access or actual DwC-A downloads.

test_that("reflora_download() skips collections with zero records", {
  fake_info <- list(list(character(0)), c("zero_herb"), c("ZERO"))
  fake_row <- list(c("1.0", "2020-01-01 00:00", "0"), "Nobody", NA_character_,
                    "Nowhere", "https://ipt.jbrj.gov.br/reflora/resource?r=zero_herb", FALSE)

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_row,
    .package = "refloraR"
  )

  tmp_dir <- file.path(tempdir(), "reflora_download_mock_zero")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  reflora_download(verbose = FALSE, dir = tmp_dir)
  expect_equal(list.files(tmp_dir), character(0))
})

test_that("reflora_download() does not re-download an already-present collection", {
  fake_info <- list(list(character(0)), c("heph"), c("HEPH"))
  fake_row <- list(c("1.223", "2026-09-15 01:08", "28,697"), "Roberta Chacon", "rgchacon@gmail.com",
                    "Jardim Botânico de Brasília", "https://ipt.jbrj.gov.br/reflora/resource?r=heph", FALSE)

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_row,
    .package = "refloraR"
  )
  testthat::local_mocked_bindings(
    download.file = function(...) stop("network should not be reached"),
    unzip = function(...) stop("network should not be reached"),
    .package = "utils"
  )

  tmp_dir <- file.path(tempdir(), "reflora_download_mock_cached")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  # Version "1.223" -> already-downloaded folder is "dwca_heph_v1_223"
  cached_dir <- file.path(tmp_dir, "dwca_heph_v1_223")
  dir.create(cached_dir, recursive = TRUE)
  file.create(file.path(cached_dir, "occurrence.txt"))

  expect_silent(reflora_download(verbose = FALSE, dir = tmp_dir))
})

test_that("reflora_download() downloads, extracts and saves a new collection", {
  fake_info <- list(list(character(0)), c("heph"), c("HEPH"))
  fake_row <- list(c("1.223", "2026-09-15 01:08", "28,697"), "Roberta Chacon", "rgchacon@gmail.com",
                    "Jardim Botânico de Brasília", "https://ipt.jbrj.gov.br/reflora/resource?r=heph", FALSE)

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_row,
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .package = "refloraR"
  )
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      file.create(destfile)
      invisible(0L)
    },
    unzip = function(zipfile, exdir, ...) {
      dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(exdir, "occurrence.txt"))
      invisible(NULL)
    },
    .package = "utils"
  )

  tmp_dir <- file.path(tempdir(), "reflora_download_mock_full")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_message(
    reflora_download(herbarium = "HEPH", verbose = TRUE, dir = tmp_dir),
    "HEPH collection sucessfully downloaded"
  )

  extracted <- list.files(tmp_dir, recursive = TRUE)
  expect_true(any(grepl("occurrence\\.txt$", extracted)))
  expect_true(any(grepl("HEPH_Reflora\\.csv$", extracted)))
})

test_that("reflora_download() skips repatriated collections when repatriated = FALSE", {
  fake_info <- list(list(character(0)), c("k_reflora"), c("K"))
  fake_row <- list(c("1.0", "2026-01-01 00:00", "1,000"), "Someone", "someone@example.com",
                    "Royal Botanic Gardens, Kew", "https://ipt.jbrj.gov.br/reflora/resource?r=k_reflora", TRUE)

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_row,
    .package = "refloraR"
  )
  testthat::local_mocked_bindings(
    download.file = function(...) stop("network should not be reached"),
    unzip = function(...) stop("network should not be reached"),
    .package = "utils"
  )

  tmp_dir <- file.path(tempdir(), "reflora_download_mock_repatriated")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_message(
    reflora_download(repatriated = FALSE, verbose = TRUE, dir = tmp_dir),
    "Skipping repatriated collection: K"
  )
})
