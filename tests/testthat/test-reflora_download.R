test_that("reflora_download works for a search with a vector of herbarium acronyms", {
  skip_on_cran()
  skip_if_offline()

  reflora_download(herbarium = c("ALCB", "HUEFS"),
                   verbose = FALSE,
                   dir = "reflora_download")

  filenames <- list.files("reflora_download")

  alcb <- list.files(paste0("reflora_download/", filenames[1]))
  huefs <- list.files(paste0("reflora_download/", filenames[1]))

  expect_equal(dir.exists("reflora_download"), TRUE)
  expect_equal(length(list.files("reflora_download"))==2, TRUE)
  expect_equal(length(alcb)==6, TRUE)
  expect_equal(length(alcb)==length(huefs), TRUE)
})

test_that("reflora_download works for a search with a vector of herbarium acronyms", {
  skip_on_cran()
  skip_if_offline()

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
    expect_true(length(files) >= 3)  # expecting at least metadata + occurrence.txt
  }

  # Check that version CSV exists for each
  all_csvs <- unlist(lapply(downloaded_dirs, function(x) list.files(x, pattern = "_Reflora_version.csv", full.names = TRUE)))
  expect_equal(length(all_csvs), 2)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})
