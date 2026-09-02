test_that("reflora_summary works for full search (herbarium = NULL) or with a vector of herbarium acronyms", {
  skip_if_no_reflora()
  res_ex <- reflora_summary(verbose = FALSE,
                            save = FALSE,
                            dir = "reflora_summary")

  res_ex_some <- reflora_summary(herbarium = c("ALCB"),
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
})


test_that("reflora_summary saves file when save = TRUE", {
  skip_if_no_reflora()
  temp_dir <- tempdir()
  res <- reflora_summary(herbarium = c("RB"),
                         verbose = FALSE,
                         save = TRUE,
                         dir = temp_dir)

  output_path <- file.path(temp_dir, "reflora_summary.csv")
  expect_true(file.exists(output_path))
  unlink(output_path)
})


test_that("reflora_summary fails with invalid herbarium code", {
  skip_if_no_reflora()
  expect_error(
    reflora_summary(herbarium = "FAKE",
                    verbose = FALSE,
                    save = FALSE)
  )
})

test_that("reflora_summary works with trailing slash in dir", {
  skip_if_no_reflora()
  temp_dir <- file.path(tempdir(), "reflora_summary_dir/")
  res <- reflora_summary(herbarium = "RB",
                         verbose = FALSE,
                         save = TRUE,
                         dir = temp_dir)

  expect_s3_class(res, "data.frame")
  expect_true(file.exists(file.path(gsub("/$", "", temp_dir), "reflora_summary.csv")))

  unlink(temp_dir, recursive = TRUE)
})


test_that("reflora_summary prints expected verbose messages", {
  skip_if_no_reflora()
  local_edition(3)  # Required for proper testthat behavior under covr
  expect_message(
    reflora_summary(herbarium = "RB",
                    verbose = TRUE,
                    save = FALSE),
    regexp = "Summarizing specimen collections of RB 1/1"
  )
})


test_that("reflora_summary returns NA if contact email is missing", {
  skip_if_no_reflora()
  df <- reflora_summary(herbarium = "RB",
                        verbose = FALSE,
                        save = FALSE)
  expect_true(is.na(df$hasEmail[1]) || is.character(df$hasEmail[1]))
})


test_that("reflora_summary returns Records column as numeric", {
  skip_if_no_reflora()
  df <- reflora_summary(herbarium = "RB",
                        verbose = FALSE,
                        save = FALSE)
  expect_type(df$Records, "double")
})


test_that("herbarium validation is local and deterministic", {
  expect_null(.validate_herbarium(NULL))
  expect_equal(.validate_herbarium(c(" alcB ", "huefs", "ALCB")), c("ALCB", "HUEFS"))

  expect_error(.validate_herbarium(character()), "non-empty character vector")
  expect_error(.validate_herbarium(c("ALCB", NA_character_)), "non-empty character vector")
  expect_error(.validate_herbarium("ALCB!"), "letters, numbers, or hyphens")
})


test_that("Reflora resource keys and collection codes are parsed", {
  dataset <- list(
    endpoints = list(
      list(
        type = "DWC_ARCHIVE",
        url = "https://ipt.jbrj.gov.br/reflora/archive.do?r=huefs_herbarium"
      )
    )
  )

  expect_equal(.reflora_resource_key(dataset), "huefs_herbarium")
  expect_equal(.resource_collection_code("huefs_herbarium"), "HUEFS")
  expect_equal(.resource_collection_code("nyh_repatriated"), "NY")
})


test_that("contact selection prioritizes the administrative contact", {
  dataset <- list(
    contacts = list(
      list(type = "METADATA_AUTHOR", firstName = "Meta", lastName = "Author"),
      list(
        type = "ADMINISTRATIVE_POINT_OF_CONTACT",
        firstName = "Herbarium",
        lastName = "Curator",
        email = c("curator@example.org", "collection@example.org")
      )
    )
  )

  contact <- .dataset_contact(dataset)

  expect_equal(.contact_name(contact), "Herbarium Curator")
  expect_equal(
    .collapse_character(contact$email),
    "curator@example.org; collection@example.org"
  )
})


test_that("version and repatriation status are parsed from Registry metadata", {
  dataset <- list(
    title = "K herbarium - Amostras Brasileiras - Herbário Virtual REFLORA",
    citation = list(
      text = "Example (2026). Example dataset. Version 1.176. Publisher."
    )
  )

  expect_equal(.dataset_version(dataset), "1.176")
  expect_true(.dataset_is_repatriated(dataset))
})


test_that("a complete Registry dataset becomes one stable summary row", {
  dataset <- list(
    key = "9e54731f-ca52-4389-95cb-e4feedc7593e",
    title = paste(
      "CEN herbarium - Embrapa Recursos Genéticos e Biotecnologia",
      "- Herbário Virtual REFLORA"
    ),
    pubDate = "2026-08-01T00:00:00.000+00:00",
    citation = list(text = "Cavalcanti T (2026). CEN. Version 1.176."),
    contacts = list(
      list(
        type = "ADMINISTRATIVE_POINT_OF_CONTACT",
        firstName = "Taciana",
        lastName = "Cavalcanti",
        organization = "EMBRAPA",
        email = "curator@example.org"
      )
    ),
    endpoints = list(
      list(
        type = "DWC_ARCHIVE",
        url = "https://ipt.jbrj.gov.br/reflora/archive.do?r=cen"
      ),
      list(
        type = "EML",
        url = "https://ipt.jbrj.gov.br/reflora/eml.do?r=cen"
      )
    )
  )

  row <- .dataset_summary_row(dataset, records = 126419)

  expect_s3_class(row, "data.frame")
  expect_equal(nrow(row), 1L)
  expect_equal(row$collectionCode, "CEN")
  expect_equal(row$rightsHolder, "EMBRAPA")
  expect_equal(row$Version, "1.176")
  expect_equal(row$Records, 126419)
  expect_match(row$Reflora_URL, "resource\\?r=cen$")
  expect_equal(row$datasetKey, dataset$key)
})


test_that("record-count failure is isolated", {
  local_mocked_bindings(
    .gbif_record_count = function(dataset_key) {
      stop("temporary API error")
    },
    .package = "refloraR"
  )

  value <- .safe_gbif_record_count("example-key")

  expect_true(is.na(value))
  expect_match(attr(value, "error"), "temporary API error")
})


test_that("the live Registry still exposes REFLORA datasets", {
  skip_on_cran()
  skip_if_offline("api.gbif.org")

  datasets <- .get_installation_datasets(limit = 20L)

  expect_gt(length(datasets), 0L)
  expect_true(any(vapply(datasets, function(dataset) {
    identical(.scalar_character(dataset$installationKey), .REFLORA_INSTALLATION_KEY)
  }, logical(1))))
})


test_that("installation and network datasets are combined and deduplicated", {
  installation <- list(
    list(key = "dataset-a", title = "A herbarium - Reflora"),
    list(key = "dataset-b", title = "B herbarium - Reflora")
  )
  network <- list(
    list(key = "dataset-b", title = "B herbarium - Reflora"),
    list(key = "dataset-c", title = "C - External collection")
  )

  local_mocked_bindings(
    .get_installation_datasets = function(...) installation,
    .get_network_datasets = function(...) network,
    .package = "refloraR"
  )

  datasets <- .get_reflora_datasets()
  keys <- vapply(datasets, function(x) x$key, character(1))

  expect_equal(keys, c("dataset-a", "dataset-b", "dataset-c"))
})


test_that("network constituent pagination is supported", {
  calls <- 0L

  local_mocked_bindings(
    .gbif_get = function(path, query = list(), timeout = 30) {
      calls <<- calls + 1L

      expect_equal(
        path,
        paste0("/network/", .REFLORA_NETWORK_KEY, "/constituents")
      )

      if (calls == 1L) {
        return(list(
          results = list(list(key = "dataset-a")),
          endOfRecords = FALSE
        ))
      }

      list(
        results = list(list(key = "dataset-b")),
        endOfRecords = TRUE
      )
    },
    .package = "refloraR"
  )

  datasets <- .get_network_datasets(limit = 1L)

  expect_equal(length(datasets), 2L)
  expect_equal(calls, 2L)
})


test_that("collection codes fall back to dataset titles", {
  expect_equal(
    .title_collection_code("RB - Coleção do Herbário do Jardim Botânico"),
    "RB"
  )
  expect_equal(
    .title_collection_code("CEN herbarium - Embrapa"),
    "CEN"
  )
  expect_equal(
    .title_collection_code("Herbarium dataset from Example University (XYZ)"),
    "XYZ"
  )
  expect_true(is.na(.title_collection_code("Dataset without an acronym")))
})


test_that("Reflora IPT resource keys still take precedence", {
  dataset <- list(
    title = "A title containing a different acronym (ABC)",
    endpoints = list(
      list(
        type = "DWC_ARCHIVE",
        url = "https://ipt.jbrj.gov.br/reflora/archive.do?r=nyh"
      )
    )
  )

  expect_equal(.dataset_collection_code(dataset), "NY")
})


test_that("external network datasets retain their own endpoints", {
  dataset <- list(
    key = "external-dataset-key",
    title = "XYZ - Example University Herbarium",
    publishingCountry = "FR",
    publishingOrganizationTitle = "Example University",
    pubDate = "2026-08-01T00:00:00.000+00:00",
    citation = list(text = "Example (2026). XYZ. Version 2.1."),
    contacts = list(
      list(
        type = "ADMINISTRATIVE_POINT_OF_CONTACT",
        firstName = "Example",
        lastName = "Curator",
        organization = "Example University",
        country = "FR",
        email = "curator@example.org"
      )
    ),
    endpoints = list(
      list(
        type = "DWC_ARCHIVE",
        url = "https://data.example.org/xyz.zip"
      ),
      list(
        type = "EML",
        url = "https://data.example.org/xyz.xml"
      )
    )
  )

  row <- .dataset_summary_row(dataset, records = 1200)

  expect_equal(row$collectionCode, "XYZ")
  expect_true(row$Repatriated)
  expect_equal(row$DwCA_URL, "https://data.example.org/xyz.zip")
  expect_equal(row$EML_URL, "https://data.example.org/xyz.xml")
  expect_true(is.na(row$Reflora_URL))
  expect_true(is.na(row$resourceKey))
})


test_that("network dataset Reflora identifiers are retained", {
  dataset <- list(
    title = "XYZ - Example Herbarium",
    identifiers = list(
      list(identifier = "https://reflora.jbrj.gov.br/example/xyz")
    ),
    endpoints = list()
  )

  expect_equal(
    .dataset_reflora_url(dataset),
    "https://reflora.jbrj.gov.br/example/xyz"
  )
})


test_that("the live Reflora network exposes constituent datasets", {
  skip_on_cran()
  skip_if_offline("api.gbif.org")

  datasets <- .get_network_datasets(limit = 1000L)

  expect_gt(length(datasets), 0L)
  expect_true(all(vapply(datasets, function(dataset) {
    !is.na(.scalar_character(dataset$key))
  }, logical(1))))
})
