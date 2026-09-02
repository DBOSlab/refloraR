#' Summarize plant specimen records through Reflora Virtual Herbarium IPT
#'
#' @author Domingos Cardoso
#'
#' @description Summarize current available plant specimen records at
#' \href{https://ipt.jbrj.gov.br/reflora/}{Reflora Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#' Reflora aggregates collections from both Brazilian and international herbaria
#' that hold Brazilian specimens. In this context, 'digital repatriation' refers
#' to making high-resolution images and associated specimen metadata openly
#' accessible through a Brazilian public infrastructure (HVR/IPT), even when the
#' physical specimens remain curated in the holding herbarium. The
#' \code{reflora_summary()} output includes a repatriation status field to help
#' users identify these digitally repatriated collections.
#'
#' The `Records` column reports the number of occurrences currently indexed by
#' GBIF, which can differ temporarily from the number of rows in the newest IPT
#' archive while GBIF is crawling or processing a newly published version.
#'
#' @usage
#' reflora_summary(herbarium = NULL,
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 records = c("gbif", "none"),
#'                 dir = "reflora_summary")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records for
#' all Reflora-hosted herbaria.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when
#' summarizing specimen records will not be printed in the console in full.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param records Character. Use `"gbif"` to request indexed occurrence counts,
#'   or `"none"` to skip those requests and return `NA` in `Records`.
#'
#' @param dir Pathway to the computer's directory, where the table-formatted
#' summary will be saved. The default is to create a directory named
#'  \code{reflora_summary}.
#'
#' @return A \code{data.frame} with one row per herbarium collection and the
#' following columns:
#' \describe{
#'   \item{collectionCode}{Character. Herbarium acronym (collection code) as
#'   registered in the Reflora Virtual Herbarium.}
#'
#'   \item{rightsHolder}{Character. Institution responsible for the collection,
#'   as reported in the IPT metadata.}
#'
#'   \item{Repatriated}{Logical. Indicates whether the herbarium corresponds to
#'   a repatriated collection (i.e., Brazilian specimens digitized from foreign
#'   institutions such as US, NY, K, etc.).}
#'
#'   \item{contactPoint}{Character. Name of the contact person or curator
#'   associated with the collection.}
#'
#'   \item{hasEmail}{Character. Contact email address for the contactPoint,
#'   when available.}
#'
#'   \item{Version}{Character. Current dataset version number available in
#'   the IPT repository.}
#'
#'   \item{Published.on}{Character. Date and hour on which the current dataset
#'    version was published in the IPT.}
#'
#'   \item{Records}{Numeric. Total number of specimen records currently
#'   available for the collection.}
#'
#'   \item{Reflora_URL}{Character. Direct URL to the collection page in the
#'   Reflora Virtual Herbarium.}
#' }
#'
#' @details
#' Discovery and metadata come from the GBIF Registry API. The function takes
#' the union of datasets hosted by the Reflora IPT installation and datasets in
#' the Reflora GBIF network, and removes duplicates using the GBIF dataset UUID.
#' Reflora IPT URLs and external dataset endpoints are returned as identifiers
#' but are not opened by this function. A failure in an individual GBIF
#' occurrence-count request therefore produces `NA` only for that dataset
#' rather than aborting the entire summary.
#'
#' @examples
#' \dontrun{
#' reflora_summary()
#'
#' reflora_summary(
#'   herbarium = c("ALCB", "HUEFS"),
#'   save = FALSE
#' )
#'
#' # Faster metadata-only summary
#' reflora_summary(records = "none", save = FALSE)
#' }
#'
#' @seealso \code{\link{reflora_download}}
#'
#' @importFrom httr2 request req_url_query req_user_agent req_timeout req_perform resp_body_json
#' @importFrom utils URLdecode
#'
#' @export

reflora_summary <- function(herbarium = NULL,
                            verbose = TRUE,
                            save = TRUE,
                            records = c("gbif", "none"),
                            dir = "reflora_summary") {

  herbarium <- .validate_herbarium(herbarium)
  verbose <- .validate_flag(verbose, "verbose")
  save <- .validate_flag(save, "save")
  records <- match.arg(records)

  # dir check
  dir <- .arg_check_dir(dir)

  if (verbose) {
    message("Retrieving Reflora datasets from the GBIF Registry...")
  }

  datasets <- .get_reflora_datasets()

  if (length(datasets) == 0L) {
    stop(
      "No datasets were returned for the REFLORA IPT installation.",
      call. = FALSE
    )
  }

  # Some Registry list responses may omit nested metadata. Fetch full records
  # only for those datasets, rather than issuing an extra request for every row.
  datasets <- lapply(datasets, function(dataset) {
    if (.dataset_needs_details(dataset)) {
      key <- .scalar_character(dataset$key)
      if (!is.na(key)) {
        return(.get_dataset(key))
      }
    }
    dataset
  })

  codes <- vapply(
    datasets,
    .dataset_collection_code,
    character(1)
  )

  if (!is.null(herbarium)) {
    missing_codes <- setdiff(herbarium, unique(codes[!is.na(codes)]))

    if (length(missing_codes) > 0L) {
      stop(
        "The following herbaria were not found in the Reflora Registry datasets: ",
        paste(missing_codes, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    datasets <- datasets[codes %in% herbarium]
  }

  rows <- vector("list", length(datasets))
  count_failures <- character()

  for (i in seq_along(datasets)) {
    dataset <- datasets[[i]]
    key <- .scalar_character(dataset$key)
    code <- .dataset_collection_code(dataset)

    if (verbose) {
      message(
        "Summarizing ",
        if (is.na(code)) "dataset" else code,
        " (", i, "/", length(datasets), ")"
      )
    }

    count <- NA_real_
    if (identical(records, "gbif") && !is.na(key)) {
      count <- .safe_gbif_record_count(key)
      count_error <- attr(count, "error", exact = TRUE)

      if (!is.null(count_error)) {
        count_failures <- c(
          count_failures,
          if (is.na(code)) key else code
        )
      }
    }

    rows[[i]] <- .dataset_summary_row(dataset, records = count)
  }

  summary_df <- do.call(rbind, rows)
  rownames(summary_df) <- NULL
  summary_df <- summary_df[
    order(summary_df$collectionCode, summary_df$resourceKey, na.last = TRUE),
    ,
    drop = FALSE
  ]
  rownames(summary_df) <- NULL

  if (length(count_failures) > 0L) {
    warning(
      "GBIF record counts could not be retrieved for: ",
      paste(unique(count_failures), collapse = ", "),
      ". Their `Records` values were set to NA.",
      call. = FALSE
    )
  }

  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = summary_df,
              verbose = verbose,
              filename = "reflora_summary",
              dir = dir)
  }

  return(summary_df)

}


# Public GBIF Registry UUID assigned to the Reflora IPT installation.
# This is an identifier, not an authentication key or secret.
.REFLORA_INSTALLATION_KEY <- "ecb15bdf-98de-4937-84f5-4f93ee9efc73"

# Public GBIF Registry UUID assigned to The Virtual Herbarium Reflora network.
# Network constituents can be published through installations other than the
# Reflora IPT installation.
.REFLORA_NETWORK_KEY <- "4b0d8edb-7504-42c4-9349-63e86c01bf97"

.GBIF_API_BASE <- "https://api.gbif.org/v1"

.null_default <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0L) default else x
}

.scalar_character <- function(x, default = NA_character_) {
  x <- unlist(.null_default(x, character()), use.names = FALSE)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]

  if (length(x) == 0L) {
    return(default)
  }

  x[[1L]]
}

.collapse_character <- function(x, default = NA_character_) {
  x <- unlist(.null_default(x, character()), use.names = FALSE)
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) {
    return(default)
  }

  paste(x, collapse = "; ")
}

.validate_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  x
}

.validate_herbarium <- function(herbarium) {
  if (is.null(herbarium)) {
    return(NULL)
  }

  if (!is.character(herbarium) ||
      length(herbarium) == 0L ||
      anyNA(herbarium) ||
      any(!nzchar(trimws(herbarium)))) {
    stop(
      "`herbarium` must be NULL or a non-empty character vector of herbarium codes.",
      call. = FALSE
    )
  }

  herbarium <- unique(toupper(trimws(herbarium)))

  if (any(!grepl("^[A-Z0-9-]+$", herbarium))) {
    stop(
      "Every `herbarium` value must contain only letters, numbers, or hyphens.",
      call. = FALSE
    )
  }

  herbarium
}

.gbif_get <- function(path, query = list(), timeout = 30) {
  req <- httr2::request(paste0(.GBIF_API_BASE, path))

  if (length(query) > 0L) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  req <- req |>
    httr2::req_user_agent(
      "refloraR (https://github.com/DBOSlab/refloraR)"
    ) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(
      max_tries = 3L,
      retry_on_failure = TRUE
    )

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop(
        "The GBIF API request failed for `", path, "`: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

.get_installation_datasets <- function(
    installation_key = .REFLORA_INSTALLATION_KEY,
    limit = 1000L) {
  offset <- 0L
  datasets <- list()

  repeat {
    page <- .gbif_get(
      paste0("/installation/", installation_key, "/dataset"),
      query = list(limit = limit, offset = offset)
    )

    batch <- .null_default(page$results, list())
    datasets <- c(datasets, batch)

    if (length(batch) == 0L || isTRUE(page$endOfRecords)) {
      break
    }

    offset <- offset + length(batch)
  }

  datasets
}

.get_network_datasets <- function(
    network_key = .REFLORA_NETWORK_KEY,
    limit = 1000L) {
  offset <- 0L
  datasets <- list()

  repeat {
    page <- .gbif_get(
      paste0("/network/", network_key, "/constituents"),
      query = list(limit = limit, offset = offset)
    )

    batch <- .null_default(page$results, list())
    datasets <- c(datasets, batch)

    if (length(batch) == 0L || isTRUE(page$endOfRecords)) {
      break
    }

    offset <- offset + length(batch)
  }

  datasets
}

.get_reflora_datasets <- function() {
  installation_datasets <- .get_installation_datasets()
  network_datasets <- .get_network_datasets()
  datasets <- c(installation_datasets, network_datasets)

  if (length(datasets) == 0L) {
    return(list())
  }

  keys <- vapply(
    datasets,
    function(dataset) .scalar_character(dataset$key),
    character(1)
  )

  # Keep the first copy of every GBIF dataset. Installation datasets are placed
  # first because they generally already contain the Reflora IPT endpoints.
  valid <- !is.na(keys) & nzchar(keys)
  datasets[valid & !duplicated(keys)]
}

.get_dataset <- function(key) {
  .gbif_get(paste0("/dataset/", key))
}

.dataset_endpoint <- function(dataset, type) {
  endpoints <- .null_default(dataset$endpoints, list())

  if (length(endpoints) == 0L) {
    return(NA_character_)
  }

  endpoint_types <- vapply(
    endpoints,
    function(x) .scalar_character(x$type),
    character(1)
  )

  position <- which(endpoint_types == type)[1L]

  if (is.na(position)) {
    return(NA_character_)
  }

  .scalar_character(endpoints[[position]]$url)
}

.reflora_resource_key <- function(dataset) {
  candidates <- c(
    .dataset_endpoint(dataset, "DWC_ARCHIVE"),
    .dataset_endpoint(dataset, "EML")
  )

  identifiers <- .null_default(dataset$identifiers, list())
  if (length(identifiers) > 0L) {
    candidates <- c(
      candidates,
      vapply(
        identifiers,
        function(x) .scalar_character(x$identifier),
        character(1)
      )
    )
  }

  candidates <- candidates[
    !is.na(candidates) &
      grepl("ipt\\.jbrj\\.gov\\.br/reflora", candidates, ignore.case = TRUE)
  ]

  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  match <- regexec("[?&]r=([^&#]+)", candidates[[1L]], perl = TRUE)
  value <- regmatches(candidates[[1L]], match)[[1L]]

  if (length(value) < 2L) {
    return(NA_character_)
  }

  utils::URLdecode(value[[2L]])
}

.resource_collection_code <- function(resource_key) {
  if (is.na(resource_key) || !nzchar(resource_key)) {
    return(NA_character_)
  }

  code <- toupper(sub("_.*$", "", resource_key))

  # Historical Reflora resource name; the official herbarium code is NY.
  if (identical(code, "NYH")) {
    code <- "NY"
  }

  code
}

.title_collection_code <- function(title) {
  title <- .scalar_character(title)

  if (is.na(title)) {
    return(NA_character_)
  }

  patterns <- c(
    "^\\s*([A-Z][A-Z0-9-]{0,14})\\s*(?:[-\u2013\u2014:]|\\b(?:herbarium|herb\u00E1rio)\\b)",
    "\\b(?:herbarium|herb\u00E1rio)\\s+([A-Z][A-Z0-9-]{0,14})\\b",
    "\\(([A-Z][A-Z0-9-]{0,14})\\)"
  )

  excluded <- c(
    "THE", "HERBARIUM", "HERBARIO", "COLLECTION", "DATASET", "FLORA"
  )

  for (pattern in patterns) {
    match <- regexec(
      pattern,
      title,
      ignore.case = TRUE,
      perl = TRUE
    )
    value <- regmatches(title, match)[[1L]]

    if (length(value) >= 2L) {
      code <- toupper(value[[2L]])

      if (nzchar(code) && !code %in% excluded) {
        return(code)
      }
    }
  }

  NA_character_
}

.dataset_collection_code <- function(dataset) {
  resource_code <- .resource_collection_code(
    .reflora_resource_key(dataset)
  )

  if (!is.na(resource_code)) {
    return(resource_code)
  }

  .title_collection_code(dataset$title)
}

.dataset_reflora_url <- function(dataset, resource_key = NULL) {
  if (is.null(resource_key)) {
    resource_key <- .reflora_resource_key(dataset)
  }

  if (!is.na(resource_key)) {
    return(
      paste0(
        "https://ipt.jbrj.gov.br/reflora/resource?r=",
        utils::URLencode(resource_key, reserved = TRUE)
      )
    )
  }

  identifiers <- .null_default(dataset$identifiers, list())
  candidates <- character()

  if (length(identifiers) > 0L) {
    candidates <- vapply(
      identifiers,
      function(x) .scalar_character(x$identifier),
      character(1)
    )
  }

  candidates <- c(
    candidates,
    .scalar_character(dataset$homepage)
  )
  candidates <- candidates[
    !is.na(candidates) &
      grepl(
        "(^https?://)?([^/]+\\.)?reflora\\.jbrj\\.gov\\.br|ipt\\.jbrj\\.gov\\.br/reflora",
        candidates,
        ignore.case = TRUE
      )
  ]

  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  candidates[[1L]]
}

.dataset_contact <- function(dataset) {
  contacts <- .null_default(dataset$contacts, list())

  if (length(contacts) == 0L) {
    return(list())
  }

  priority <- c(
    "ADMINISTRATIVE_POINT_OF_CONTACT",
    "ORIGINATOR",
    "METADATA_AUTHOR",
    "POINT_OF_CONTACT",
    "PUBLISHER",
    "CUSTODIAN"
  )

  contact_types <- vapply(
    contacts,
    function(x) .scalar_character(x$type, ""),
    character(1)
  )

  for (role in priority) {
    position <- which(contact_types == role)[1L]
    if (!is.na(position)) {
      return(contacts[[position]])
    }
  }

  contacts[[1L]]
}

.contact_name <- function(contact) {
  components <- c(
    .scalar_character(contact$firstName),
    .scalar_character(contact$lastName)
  )
  components <- components[!is.na(components)]

  if (length(components) > 0L) {
    return(paste(components, collapse = " "))
  }

  .scalar_character(contact$name)
}

.dataset_rights_holder <- function(dataset, contact) {
  organization <- .scalar_character(contact$organization)

  if (!is.na(organization) &&
      !grepl("Jardim Bot[a\u00E2]nico|JBRJ", organization, ignore.case = TRUE)) {
    return(organization)
  }

  title <- .scalar_character(dataset$title)
  if (is.na(title)) {
    return(organization)
  }

  holder <- title
  holder <- sub(
    "^[A-Z0-9-]+\\s+(herbarium|herb\u00E1rio)\\s*-\\s*",
    "",
    holder,
    ignore.case = TRUE
  )
  holder <- sub(
    "\\s*-\\s*(Herb\u00E1rio Virtual REFLORA|REFLORA Virtual Herbarium).*$",
    "",
    holder,
    ignore.case = TRUE
  )
  holder <- sub(
    "\\s*-\\s*(Amostras Brasileiras|Brazilian specimens).*$",
    "",
    holder,
    ignore.case = TRUE
  )
  holder <- trimws(holder)

  if (nzchar(holder)) holder else organization
}

.dataset_is_repatriated <- function(dataset) {
  text <- paste(
    c(
      .scalar_character(dataset$title, ""),
      .scalar_character(dataset$description, "")
    ),
    collapse = " "
  )

  explicit_repatriation <- grepl(
    "Amostras\\s+Brasileiras|Brazilian\\s+(specimens|samples)",
    text,
    ignore.case = TRUE
  )

  if (explicit_repatriation) {
    return(TRUE)
  }

  country <- toupper(.scalar_character(dataset$publishingCountry, ""))

  if (!nzchar(country)) {
    contact <- .dataset_contact(dataset)
    country <- toupper(.scalar_character(contact$country, ""))
  }

  if (!nzchar(country)) {
    return(FALSE)
  }

  !country %in% c("BR", "BRA", "BRAZIL")
}

.dataset_version <- function(dataset) {
  citation <- dataset$citation
  citation_text <- if (is.list(citation)) {
    .scalar_character(citation$text)
  } else {
    .scalar_character(citation)
  }

  if (is.na(citation_text)) {
    return(NA_character_)
  }

  match <- regexec(
    "\\bVersion\\s+([0-9]+(?:\\.[0-9]+)*)",
    citation_text,
    ignore.case = TRUE,
    perl = TRUE
  )
  value <- regmatches(citation_text, match)[[1L]]

  if (length(value) < 2L) NA_character_ else value[[2L]]
}

.gbif_record_count <- function(dataset_key) {
  result <- .gbif_get(
    "/occurrence/search",
    query = list(dataset_key = dataset_key, limit = 0L)
  )

  count <- .null_default(result$count, NA_real_)
  as.numeric(count[[1L]])
}

.safe_gbif_record_count <- function(dataset_key) {
  tryCatch(
    .gbif_record_count(dataset_key),
    error = function(e) {
      structure(NA_real_, error = conditionMessage(e))
    }
  )
}

.dataset_needs_details <- function(dataset) {
  is.null(dataset$endpoints) ||
    is.null(dataset$contacts) ||
    is.null(dataset$citation)
}

.dataset_summary_row <- function(dataset, records = NA_real_) {
  dataset_key <- .scalar_character(dataset$key)
  resource_key <- .reflora_resource_key(dataset)
  collection_code <- .dataset_collection_code(dataset)
  contact <- .dataset_contact(dataset)
  dwca_url <- .dataset_endpoint(dataset, "DWC_ARCHIVE")
  eml_url <- .dataset_endpoint(dataset, "EML")
  reflora_url <- .dataset_reflora_url(dataset, resource_key)

  data.frame(
    collectionCode = collection_code,
    rightsHolder = .dataset_rights_holder(dataset, contact),
    Repatriated = .dataset_is_repatriated(dataset),
    contactPoint = .contact_name(contact),
    hasEmail = .collapse_character(contact$email),
    Version = .dataset_version(dataset),
    Published.on = .scalar_character(dataset$pubDate),
    Records = records,
    Reflora_URL = reflora_url,
    datasetKey = dataset_key,
    resourceKey = resource_key,
    DwCA_URL = dwca_url,
    EML_URL = eml_url,
    GBIF_URL = if (is.na(dataset_key)) {
      NA_character_
    } else {
      paste0("https://www.gbif.org/dataset/", dataset_key)
    },
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
