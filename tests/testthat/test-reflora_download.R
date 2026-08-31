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
