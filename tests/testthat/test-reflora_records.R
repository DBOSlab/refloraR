test_that("reflora_records basic usage returns a data.frame", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    save = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
})

test_that("reflora_records handles empty taxon search", {
  expect_error(
    reflora_records(
    herbarium = "RB",
    taxon = "Fakeplantus invalidus",
    save = FALSE,
    verbose = FALSE)
  )
})

test_that("reflora_records applies state and year filters", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    state = c("Bahia", "Minas Gerais"),
    recordYear = c("2000", "2024"),
    save = FALSE,
    verbose = FALSE
  )

  expect_true(all(result$stateProvince %in% c("Bahia", "Minas Gerais")))
  expect_true(all(as.numeric(result$year) >= 2000 & as.numeric(result$year) <= 2024))
})

test_that("reflora_records reorders columns properly", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    reorder = c("year", "herbarium"),
    save = FALSE,
    verbose = FALSE
  )

  expect_true("year" %in% names(result))
  expect_true("institutionCode" %in% names(result))
})

test_that("reflora_records saves file when save = TRUE", {
  temp_dir <- tempdir()
  test_file <- "test_output"

  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    dir = temp_dir,
    filename = test_file,
    save = TRUE,
    verbose = FALSE
  )

  output_path <- file.path(temp_dir, paste0(test_file, ".csv"))
  expect_true(file.exists(output_path))

  # Clean up created file
  unlink(output_path)
})

test_that(".check_taxon_match handles valid and invalid taxa", {
  df <- data.frame(
    family = c("Fabaceae", "Rosaceae"),
    genus = c("Luetzelburgia", "Rosa"),
    taxonName = c("Luetzelburgia auriculata", "Rosa canina"),
    stringsAsFactors = FALSE
  )

  expect_silent(
    .check_taxon_match(df, c("Fabaceae", "Luetzelburgia"), verbose = FALSE)
  )

  expect_error(
    .check_taxon_match(df, c("Fakeplantus"), verbose = FALSE),
    "must contain at least one name"
  )

  expect_message(
    .check_taxon_match(df, c("Fabaceae", "Unknownus"), verbose = TRUE),
    "not found"
  )
})

test_that(".check_year_match handles valid and invalid years", {
  df <- data.frame(
    year = c(1999, 2005, 2020),
    stringsAsFactors = FALSE
  )

  expect_silent(
    .check_year_match(df, c("2005", "2020"), verbose = FALSE)
  )

  expect_error(
    .check_year_match(df, c("1800", "1801"), verbose = FALSE),
    "must contain at least one year"
  )

  expect_message(
    .check_year_match(df, c("2005", "3000"), verbose = TRUE),
    "not found"
  )
})

test_that(".check_state_match handles valid and invalid states", {
  df <- data.frame(
    stateProvince = c("Bahia", "Minas Gerais", "São Paulo"),
    stringsAsFactors = FALSE
  )

  expect_silent(
    .check_state_match(df, c("Bahia", "Minas Gerais"), verbose = FALSE)
  )

  expect_error(
    .check_state_match(df, c("ZZ", "XX"), verbose = FALSE),
    "must contain at least one name"
  )

  expect_message(
    .check_state_match(df, c("Minas Gerais", "ZZ"), verbose = TRUE),
    "not found"
  )
})


