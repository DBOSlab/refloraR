test_that(".arg_check_dir handles valid and invalid input", {
  expect_equal(.arg_check_dir("my/path"), "my/path")
  expect_equal(.arg_check_dir("my/path/"), "my/path")
  expect_error(.arg_check_dir(123), "The argument dir should be a character")
})

test_that(".arg_check_path detects missing or invalid folders", {
  temp <- tempdir()
  empty_dir <- file.path(temp, "empty_folder")
  dir.create(empty_dir, showWarnings = FALSE)

  # 1. Invalid path (not a dir)
  expect_error(.arg_check_path(42, character(), list()), "should be a character")

  # 2. Dir doesn't exist
  expect_error(.arg_check_path("not_exist", character(), list()), "There is no folder")

  # 3. No dwca folders
  expect_error(.arg_check_path(empty_dir, "folder", list()), "no REFLORA-downloaded dwca")

  # 4. dwca folder but empty
  fake_dwca <- file.path(temp, "dwca_example")
  dir.create(fake_dwca, showWarnings = FALSE)
  expect_error(.arg_check_path(temp, basename(fake_dwca), list(list())), "fully empty")

  # 5. Missing key files
  expect_error(.arg_check_path(temp, basename(fake_dwca), list(list("random.txt"))),
               "files are missing")
})

test_that(".arg_check_recordYear detects bad input", {
  expect_silent(.arg_check_recordYear(c("2000", "2024")))
  expect_error(.arg_check_recordYear(c("2024", "2000")), "first year must be less")
  expect_error(.arg_check_recordYear(c("199", "2020")), "4-digit")
  expect_error(.arg_check_recordYear(c("2000", "2020", "2024")), "should be either a single year or a range")
})

test_that(".arg_check_state converts and validates state names", {
  expect_equal(.arg_check_state("BA"), "Bahia")
  expect_equal(.arg_check_state("Bahia"), "Bahia")
  expect_equal(.arg_check_state("São Paulo"), "São Paulo")
  expect_equal(.arg_check_state("Sao Paulo"), "São Paulo") # test with diacritic removed
  expect_error(.arg_check_state("Zanzibar"), "is not valid")
})

test_that(".arg_check_herbarium accepts and rejects herbarium acronyms", {
  mock_get_info <- function(...) list(NULL, NULL, c("RB", "ALCB"))
  assignInNamespace(".get_ipt_info", mock_get_info, ns = "refloraR")

  expect_silent(.arg_check_herbarium(c("RB", "ALCB")))
  expect_error(.arg_check_herbarium(c("FAKE")), "not found")
})

test_that(".format_acronyms formats acronym list correctly", {
  expect_equal(.format_acronyms("RB"), "'RB'")
  expect_equal(.format_acronyms(c("RB", "ALCB")), "'RB' and 'ALCB'")
  expect_equal(.format_acronyms(c("RB", "ALCB", "K")), "'RB', 'ALCB' and 'K'")
})
