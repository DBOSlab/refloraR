test_that("reflora_indets returns a data.frame and filters by level FAMILY", {
  df <- reflora_indets(
    level = "FAMILY",
    taxon = "Fabaceae",
    herbarium = "RB",
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(df, "data.frame")
  expect_true(all(df$taxonRank %in% c("family", "FAMILY", "FAMILIA")))
})


test_that("reflora_indets filters by level GENUS", {
  df <- reflora_indets(
    level = "GENUS",
    taxon = "Fabaceae",
    herbarium = "RB",
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(df, "data.frame")
  expect_true(all(df$taxonRank %in% c("genus", "GENERO")))
})


test_that("reflora_indets filters by year range and state", {
  df <- reflora_indets(
    level = "FAMILY",
    taxon = "Fabaceae",
    herbarium = "RB",
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
    taxon = "Fabaceae",
    herbarium = "RB",
    dir = tmpdir,
    filename = outfile,
    save = TRUE,
    verbose = FALSE
  )
  expected_file <- file.path(tmpdir, paste0(outfile, ".csv"))
  expect_true(file.exists(expected_file))
  unlink(expected_file)
})


test_that("reflora_indets returns more rows when level is NULL (all indets)", {
  all_levels <- reflora_indets(
    level = NULL,
    taxon = "Fabaceae",
    herbarium = "RB",
    verbose = FALSE,
    save = FALSE
  )
  only_family <- reflora_indets(
    level = "FAMILY",
    taxon = "Fabaceae",
    herbarium = "RB",
    verbose = FALSE,
    save = FALSE
  )
  expect_gt(nrow(all_levels), nrow(only_family))
})


test_that("reflora_indets uses updates = FALSE with provided path", {
  temp_path <- tempdir()
  reflora_download(herbarium = "RB", dir = temp_path, verbose = FALSE)

  df <- reflora_indets(
    level = "FAMILY",
    taxon = "Fabaceae",
    herbarium = "RB",
    path = temp_path,
    updates = FALSE,
    save = FALSE,
    verbose = FALSE
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
    taxon = "Fabaceae",
    herbarium = "ALCB",
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
    taxon = "Fabaceae",
    herbarium = "RB",
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
    taxon = "Fabaceae",
    herbarium = "RB",
    dir = tmpdir,
    save = TRUE,
    verbose = FALSE
  )

  expect_true(dir.exists(tmpdir))
  unlink(tmpdir, recursive = TRUE)
})


test_that("reflora_indets handles non-matching level filter", {
  expect_error(
    reflora_indets(
      level = "SPECIES", # invalid for this function
      taxon = "Fabaceae",
      herbarium = "RB",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("reflora_indets returns empty for unknown taxon", {
  expect_error(
    reflora_indets(
      level = "FAMILY",
      taxon = "Fakeplantus",
      herbarium = "ALCB",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("reflora_indets works with no filters (all default args)", {
  df <- reflora_indets(
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(df, "data.frame")
})
