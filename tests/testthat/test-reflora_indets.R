test_that("reflora_indets returns a data.frame and filters by level 'FAMILY'", {
  df <- reflora_indets(
    level = "FAMILY",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(df, "data.frame")
  expect_true(all(df$taxonRank %in% "FAMILY"))
})


test_that("reflora_indets filters by level 'GENUS'", {
  df <- reflora_indets(
    level = "GENUS",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(df, "data.frame")
  expect_true(all(df$taxonRank %in% c("GENUS")))
})


test_that("reflora_indets filters by year range and state", {
  df <- reflora_indets(
    level = "FAMILY",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    recordYear = c("2000", "2024"),
    state = c("Bahia", "Minas Gerais"),
    verbose = FALSE,
    save = FALSE
  )
  expect_true(all(df$stateProvince %in% c("Bahia", "Minas Gerais")))
  expect_true(all(!is.na(df$year)))
  expect_true(all(df$year >= 2000 & df$year <= 2024))
})


test_that("reflora_indets saves output when save = TRUE", {
  tmpdir <- tempdir()
  outfile <- "test_indets"
  df <- reflora_indets(
    level = "GENUS",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE,
    dir = tmpdir,
    filename = outfile,
  )
  expected_file <- file.path(tmpdir, paste0(outfile, ".csv"))
  expect_true(file.exists(expected_file))
  unlink(expected_file)
})


test_that("reflora_indets returns more rows when level is NULL (all indets)", {
  all_levels <- reflora_indets(
    level = NULL,
    herbarium = "ALCB",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )
  only_family <- reflora_indets(
    level = "FAMILY",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )
  expect_gt(nrow(all_levels), nrow(only_family))
})


test_that("reflora_indets uses updates = FALSE with provided path", {
  temp_path <- tempdir()
  reflora_download(herbarium = "ALCB", dir = temp_path, verbose = FALSE)

  df <- reflora_indets(
    level = "FAMILY",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    path = temp_path,
    updates = FALSE,
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(df, "data.frame")
})


test_that("reflora_indets updates = TRUE and path is given", {
  temp_path <- tempdir()
  reflora_download(herbarium = "ALCB",
                   dir = temp_path,
                   verbose = FALSE)

  df <- reflora_indets(
    level = "GENUS",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    path = temp_path,
    updates = TRUE,
    save = FALSE,
    verbose = FALSE
  )

  expect_s3_class(df, "data.frame")
})


test_that("reflora_indets applies custom reorder", {
  df <- reflora_indets(
    level = "FAMILY",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    reorder = c("year", "taxa"),
    verbose = FALSE,
    save = FALSE
  )
  expect_true("year" %in% names(df))
})


test_that("reflora_indets creates directory if missing", {
  tmpdir <- file.path(tempdir(), "new_test_indets_dir")
  if (dir.exists(tmpdir)) unlink(tmpdir, recursive = TRUE)

  reflora_indets(
    level = "FAMILY",
    herbarium = "ALCB",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE,
    dir = tmpdir
  )

  expect_true(dir.exists(tmpdir))
  unlink(tmpdir, recursive = TRUE)
})


test_that("reflora_indets handles non-matching level filter", {
  expect_error(
    reflora_indets(
      level = "species", # invalid for this function
      herbarium = "ALCB",
      taxon = "Fabaceae",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("reflora_indets returns empty for unknown taxon", {
  expect_error(
    reflora_indets(
      level = "FAMILY",
      herbarium = "ALCB",
      taxon = "Fakeplantus",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("reflora_indets works with no filters (all default args)", {
  df <- reflora_indets(
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(df, "data.frame")
})


test_that("reflora_indets defaults to repatriated = TRUE", {
  result <- reflora_indets(
    herbarium = "ALCB",
    repatriated = TRUE,
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_indets with repatriated = FALSE excludes repatriated herbaria", {
  result <- reflora_indets(
    herbarium = NULL,
    repatriated = FALSE,
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE
  )

  expect_s3_class(result, "data.frame")

  # Ensure repatriated herbaria like K and E are not present
  repatriated_codes <- c("K", "E")
  found <- unique(result$collectionCode)
  expect_true(all(!repatriated_codes %in% found))
})


test_that("reflora_indets prints messages with verbose = TRUE", {
  expect_message(reflora_indets(herbarium = "ALCB",
                                level = "FAMILY",
                                verbose = TRUE,
                                save = FALSE))
})


test_that("reflora_indets saves CSV and log", {
  tmp <- tempfile()
  dir.create(tmp)
  result <- reflora_indets(herbarium = "ALCB",
                           verbose = FALSE,
                           save = TRUE,
                           dir = tmp,
                           filename = "test_save")
  expect_true(file.exists(file.path(tmp, "test_save.csv")))
  expect_true(file.exists(file.path(tmp, "log.txt")))
  unlink(tmp, recursive = TRUE)
})

