test_that("reflora_parse excludes repatriated herbaria when repatriated = FALSE", {
  skip_if_no_reflora()
  test_path <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(test_path)) unlink(test_path, recursive = TRUE)

  # Simulate download of both repatriated and non-repatriated herbaria
  reflora_download(herbarium = c("ALCB", "K"),
                   dir = test_path,
                   verbose = FALSE)

  # Run with repatriated = FALSE (K should be excluded)
  parsed <- reflora_parse(path = test_path,
                          herbarium = NULL,
                          repatriated = FALSE,
                          verbose = FALSE)

  collections <- names(parsed)

  # K should not be in the parsed dwca list
  expect_false(any(grepl("dwca_k_reflora", collections)))
  expect_true(any(grepl("dwca_alcb_", collections)))

  unlink(test_path, recursive = TRUE)
})


test_that("reflora_parse includes repatriated herbaria when repatriated = TRUE", {
  skip_if_no_reflora()
  test_path <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(test_path)) unlink(test_path, recursive = TRUE)

  reflora_download(herbarium = c("ALCB", "K"),
                   dir = test_path,
                   verbose = FALSE)

  parsed <- reflora_parse(path = test_path,
                          herbarium = NULL,
                          repatriated = TRUE,
                          verbose = FALSE)

  collections <- names(parsed)

  expect_true(any(grepl("dwca_k_reflora", collections)))
  expect_true(any(grepl("dwca_alcb_", collections)))

  unlink(test_path, recursive = TRUE)
})


test_that("reflora_parse emits message when skipping repatriated collections", {
  skip_if_no_reflora()
  test_path <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(test_path)) unlink(test_path, recursive = TRUE)

  reflora_download(herbarium = c("ALCB", "K"), dir = test_path, verbose = FALSE)

  expect_message(
    reflora_parse(path = test_path,
                  herbarium = NULL,
                  repatriated = FALSE,
                  verbose = TRUE),
    "Skipping repatriated collections:"
  )
  unlink(test_path, recursive = TRUE)
})


# This test mocks .get_ipt_info()/.get_herb_info() (used internally via
# reflora_summary() to know which collections are repatriated) so the
# repatriated-collection filtering and messaging logic at the top of
# reflora_parse() is covered without network access. The (empty) fixture
# folders make .arg_check_path() error right after filtering, which is
# expected and still confirms only the non-repatriated folder survived.
test_that("reflora_parse() filters out repatriated collections using mocked IPT data", {
  fake_info <- list(list(character(0), character(0)), c("heph", "k_reflora"), c("HEPH", "K"))
  fake_rows <- list(
    heph = list(c("1.0", "2026-01-01 00:00", "10"), "A", "a@example.com", "HEPH holder", "url1", FALSE),
    k_reflora = list(c("1.0", "2026-01-01 00:00", "10"), "B", "b@example.com", "K holder", "url2", TRUE)
  )
  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) fake_info,
    .get_herb_info = function(herb_URLs, ipt_metadata, i) fake_rows[[herb_URLs[i]]],
    .package = "refloraR"
  )

  tmp_dir <- file.path(tempdir(), "reflora_parse_mock")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp_dir, "dwca_heph_v1"), recursive = TRUE)
  dir.create(file.path(tmp_dir, "dwca_k_reflora_v1"), recursive = TRUE)
  # non-empty so .arg_check_path() reaches the identification/occurrence
  # check (rather than the earlier "fully empty" check) for the survivor
  file.create(file.path(tmp_dir, "dwca_heph_v1", "placeholder.txt"))

  messages <- character()
  result <- tryCatch(
    withCallingHandlers(
      reflora_parse(path = tmp_dir, repatriated = FALSE, verbose = TRUE),
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) conditionMessage(e)
  )

  expect_true(any(grepl("Skipping repatriated collections: 'K'", messages)))
  # only the non-repatriated HEPH folder survives filtering, and it then
  # fails path validation because it has no real DwC-A files (expected: we
  # are only exercising the filtering logic here, not a full parse)
  expect_true(is.character(result) &&
                grepl("identification.txt|occurrence.txt", result))
})


# This test mocks finch::dwca_read() so the full column-standardization and
# parsing pipeline of reflora_parse() is covered without a real Darwin Core
# Archive.
test_that("reflora_parse() parses mocked DwC-A folders into a named, standardized list", {
  testthat::local_mocked_bindings(
    dwca_read = function(input, ...) .fake_raw_dwca_object(basename(input)),
    .package = "finch"
  )

  tmp_dir <- file.path(tempdir(), "reflora_parse_full_mock")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  folder <- file.path(tmp_dir, "dwca_heph_v1")
  dir.create(folder, recursive = TRUE)
  file.create(file.path(folder, "identification.txt"))
  file.create(file.path(folder, "occurrence.txt"))

  parsed <- reflora_parse(path = tmp_dir, verbose = FALSE)

  expect_type(parsed, "list")
  expect_equal(names(parsed), "dwca_heph_v1")

  occ <- parsed[["dwca_heph_v1"]][["data"]][["occurrence.txt"]]
  expect_s3_class(occ, "data.frame")
  expect_equal(nrow(occ), 2)
  expect_true(all(c("family", "genus", "species", "taxonName") %in% names(occ)))
  expect_equal(occ$family, c("Fabaceae", "Fabaceae"))

  # associatedMedia (present in the fixture, unlike an all-NA column) must
  # survive .clean_media_urls_vectorized() as valid, scheme-prefixed URLs
  expect_true(all(grepl("^https://", occ$associatedMedia)))
  expect_equal(lengths(strsplit(occ$associatedMedia, "\\|")), c(1L, 2L))
})
