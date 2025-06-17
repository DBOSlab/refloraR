#' Retrieve indeterminate specimens from REFLORA collections
#'
#' @author
#' Domingos Cardoso
#'
#' @description
#' Retrieves occurrence records for indeterminate specimens (e.g., identified
#' only to family or genus level) from the
#' \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#' The function automatically downloads and parses Darwin Core Archive (DwC-A)
#' files, applies optional filters by taxon, herbarium, state, and year, and
#' exports the results if desired.
#'
#' @details
#' This function supports downloading and processing Darwin Core Archive (DwC-A)
#' files directly from the REFLORA repository. It allows for flexible filtering
#' by taxon, herbarium, locality (Brazilian states), and collection year(s). The
#' `level` parameter enables filtering for indeterminate records such as those
#' identified only to FAMILY or GENUS rank.
#'
#' The function uses helper functions like `.arg_check_herbarium()` and
#' `.filter_occur_df()` to validate inputs and refine the occurrence records. If
#' `path` is not provided, the function will automatically manage downloading and
#' storing fresh DwC-A archives.
#'
#' @note
#' - Ensure your internet connection is active if downloading is required.
#' - Some herbarium codes may not have updated records. Use `verbose = TRUE` to
#'   monitor messages.
#' - Filtering by `level` does not guarantee all indeterminates will be captured
#'   if taxonRank fields are inconsistently labeled in REFLORA's source files.
#' - For reproducible results, consider saving outputs (`save = TRUE`) and
#'   documenting your input parameters.

#'
#' @usage
#' reflora_indets(level = NULL,
#'                herbarium = NULL,
#'                taxon = NULL,
#'                state = NULL,
#'                recordYear = NULL,
#'                reorder = c("herbarium", "taxa", "collector", "area", "year"),
#'                path = NULL,
#'                updates = TRUE,
#'                verbose = TRUE,
#'                save = TRUE,
#'                dir = "reflora_indets",
#'                filename = "reflora_indets_search")
#'
#' @param level Character vector. Filter by taxonomic level. Accepted values:
#' `"FAMILY"`, `"GENUS"`, or both. Defaults to `NULL` to include all
#' indeterminate ranks.
#'
#' @param herbarium Character vector. Herbarium codes (e.g., `"RB"`, `"SP"`) in
#' uppercase. Use `NULL` to include all herbaria.
#'
#' @param taxon Character vector. Specific taxon names to filter by
#' (e.g., `"Fabaceae"`).
#'
#' @param state Character vector. Brazilian state full name or abbreviations
#' (e.g., `"BA"`, `"SP"`) to filter by locality.
#'
#' @param recordYear Character or numeric vector. A single year (e.g., `"2001"`)
#' or a range (e.g., `c("2000", "2022")`).
#'
#' @param reorder Character vector. Reorder output by columns. Defaults to:
#' `c("herbarium", "taxa", "collector", "area", "year")`.
#'
#' @param path Character. Path to existing REFLORA dwca files. If `NULL`,
#' downloads fresh data.
#'
#' @param updates Logical. If `TRUE` (default), checks for updated DwC-A files
#' from REFLORA.
#'
#' @param verbose Logical. If `TRUE` (default), prints progress messages to the
#' console.
#'
#' @param save Logical. If `TRUE` (default), saves the results to a CSV file.
#'
#' @param dir Character. Directory path to save output files. Default:
#' `"reflora_indets"`.
#'
#' @param filename Character. Name of the output file (without extension).
#' Default: `"reflora_indets_search"`.
#'
#' @return A `data.frame` containing filtered specimen records for the selected
#' indeterminate specimens and criteria.
#'
#' @seealso \code{\link{reflora_download}}
#' @seealso \code{\link{reflora_parse}}
#'
#' @examples
#' \dontrun{
#' # Retrieve indeterminate records for Fabaceae and Ochnaceae from all herbaria
#' reflora_indets(taxon = c("Fabaceae", "Ochnaceae"),
#'                level = "FAMILY",
#'                save = TRUE,
#'                dir = "reflora_indets",
#'                filename = "fabaceae_ochnaceae_records")
#'
#' # Filter by specific herbarium and state
#' reflora_indets(taxon = "Fabaceae",
#'                herbarium = "RB",
#'                state = c("BA", "MG"),
#'                recordYear = c("1990", "2022"))
#' }
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom dplyr bind_rows filter
#' @importFrom magrittr "%>%"
#'

reflora_indets <- function(level = NULL,
                           herbarium = NULL,
                           taxon = NULL,
                           state = NULL,
                           recordYear = NULL,
                           reorder = c("herbarium", "taxa", "collector", "area", "year"),
                           path = NULL,
                           updates = TRUE,
                           verbose = TRUE,
                           save = TRUE,
                           dir = "reflora_indets",
                           filename = "reflora_indets_search") {

  # herbarium check
  if (verbose & !is.null(herbarium)) {
    message("Checking whether the input herbarium code exist in the REFLORA...")
  }
  .arg_check_herbarium(herbarium)

  # state check
  .arg_check_state(state)

  # dir check
  dir <- .arg_check_dir(dir)

  # Create a new directory to save the dataframe
  # If there is no directory create one in the working directory
  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (!is.null(path)) {
    if (updates) {
      if (verbose) {
        message(paste0("Updating dwca files within '",
                       path, "'"))
      }

      # The reflora_download will get updated dwca files only if any of the current
      # versions differ from the REFLORA IPT
      reflora_download(herbarium = herbarium,
                       verbose = verbose,
                       dir = path)
    }

    # Parse REFLORA dwca files
    dwca_files <- reflora_parse(path = path,
                                herbarium = herbarium,
                                verbose = verbose)
  } else {

    # The reflora_download will get updated dwca files only if any of the current
    # versions differ from the REFLORA IPT
    reflora_download(herbarium = herbarium,
                     verbose = verbose,
                     dir = "reflora_download")

    # Parse REFLORA dwca files
    dwca_files <- reflora_parse(path = "reflora_download",
                                herbarium = herbarium,
                                verbose = verbose)
  }


  # Extract each "occurrence.txt" data frame and merge them
  occur_df <- dplyr::bind_rows(lapply(dwca_files,
                                      function(x) x[["data"]][["occurrence.txt"]]))

  if (is.null(level)) {
    # Keep only higher-rank indeterminate taxa
    indets <- c("family", "genus", "FAMILY", "GENERO", "FAMILIA", "SUB_FAMILIA",
                "TRIBO", "DIVISAO", "ORDEM", "CLASSE")
    tf <- occur_df$taxonRank %in% indets
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }

  if (level == "FAMILY") {
    indets <- c("family", "FAMILY", "FAMILIA")
    tf <- occur_df$taxonRank %in% indets
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }

  if (level == "GENUS") {
    indets <- c("genus", "GENERO")
    tf <- occur_df$taxonRank %in% indets
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }

  # Filter occurrence data
  occur_df <- .filter_occur_df(occur_df, taxon, state, recordYear, verbose)

  # Reorder the data by the order of specific columns
  occur_df <- .reorder_df(occur_df, reorder)

  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = occur_df,
              verbose = verbose,
              filename = filename,
              dir = dir)
  }

  return(occur_df)
}
