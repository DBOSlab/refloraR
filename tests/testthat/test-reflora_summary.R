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

test_that("reflora_download works for a search with a vector of herbarium acronyms", {
  temp_dir <- file.path(tempdir(), "reflora_download_test")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  reflora_download(
    herbarium = c("ALCB", "HUEFS"),
    verbose = FALSE,
    dir = temp_dir
  )

  downloaded_dirs <- list.files(temp_dir, full.names = TRUE)

  # Assert main dir was created
  expect_true(dir.exists(temp_dir))
  expect_equal(length(downloaded_dirs), 2)

  # Check each herbarium folder has content
  for (folder in downloaded_dirs) {
    files <- list.files(folder)
    expect_true(length(files) >= 3)
  }

  # Check that version CSV exists for each
  all_csvs <- unlist(lapply(downloaded_dirs, function(x) list.files(x, pattern = "_Reflora_version.csv", full.names = TRUE)))
  expect_equal(length(all_csvs), 2)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("reflora_download handles NULL herbarium input", {
  temp_dir <- file.path(tempdir(), "reflora_download_all")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  expect_silent(reflora_download(herbarium = NULL,
                                 verbose = FALSE,
                                 dir = temp_dir))
  expect_true(dir.exists(temp_dir))
  expect_true(length(list.files(temp_dir)) > 1)

  unlink(temp_dir, recursive = TRUE)
})

test_that("reflora_download returns silently with existing dwca folder", {
  temp_dir <- file.path(tempdir(), "reflora_download_cached")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)

  reflora_download(herbarium = "ALCB",
                   verbose = FALSE,
                   dir = temp_dir)

  # Call again to ensure it skips if files already exist
  expect_silent(reflora_download(herbarium = "ALCB",
                                 verbose = FALSE,
                                 dir = temp_dir))

  unlink(temp_dir, recursive = TRUE)
})

test_that("reflora_download throws error for invalid herbarium code", {
  expect_error(
    reflora_summary(herbarium = "INVALIDCODE",
                    verbose = FALSE,
                    save = FALSE)
  )
})
