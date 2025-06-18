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
