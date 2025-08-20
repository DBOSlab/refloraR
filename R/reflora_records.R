#' Retrieve taxon records from REFLORA collections
#'
#' @author
#' Carlos Calderón & Domingos Cardoso
#'
#' @description
#' Retrieve occurrence records for specific taxa from the
#' \href{https://ipt.jbrj.gov.br/reflora/}{REFLORA Virtual Herbarium},
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#' This function handles automatic download, parsing, filtering, and optional
#' saving of the occurrence data. All returned records include direct links to
#' specimen images (column \code{'bibliographicCitation'}) and, when available, high-resolution
#' download URLs (column \code{'associatedMedia'}).
#'
#' @details
#' This function processes Darwin Core Archive (DwC-A) files from REFLORA. You
#' may supply a specific path to previously downloaded files using \code{path}, or let
#' the function handle the download automatically. Filters can be applied by taxon
#' name, herbarium code, state, and year. The \code{reorder} argument allows you to
#' customize the structure of the returned data. Use \code{verbose = TRUE} to see
#' status updates. Use \code{save = TRUE} to save results to disk as a CSV file in the
#' specified directory.
#'
#' @note
#' (1) Ensure internet access for downloading data if \code{path} is not provided.
#' (2) State names may be full names or standard Brazilian two-letter codes.
#' (3) Use \code{recordYear} as a character vector to avoid coercion issues.
#' (4) This function does not apply filtering for indeterminate ranks
#'   (use \code{reflora_indets()} for that).
#' (5) \code{'bibliographicCitation'}: a clickable URL to the REFLORA specimen page
#'   where the image is displayed.
#' (6) \code{'associatedMedia'}: one or more direct URLs to the original specimen
#'   image(s) (typically high resolution). Multiple URLs may be separated by vertical bar.
#' (7) Image availability and resolution may vary by herbarium and record.
#'
#' @usage
#' reflora_records(herbarium = NULL,
#'                 repatriated = TRUE,
#'                 taxon = NULL,
#'                 state = NULL,
#'                 recordYear = NULL,
#'                 indets = TRUE,
#'                 reorder = NULL,
#'                 path = NULL,
#'                 updates = TRUE,
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_records",
#'                 filename = "reflora_records_search")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records
#' for all REFLORA-hosted herbaria.
#'
#' @param repatriated Logical. If \code{FALSE}, skips downloading records from
#' REFLORA-associated herbaria that have been repatriated. Default is \code{TRUE}.
#' Use \code{reflora_summary()} to check which collections are repatriated.
#
#' @param taxon A vector with the required taxon. It can be one or a vector of
#' multiple scientific names at family, genus or species level.
#'
#' @param state A vector with the required Brazilian state(s) (full name or acronym).
#'
#' @param recordYear A vector with the required record year or year range. For example,
#' \code{"1992"} or \code{c("1992", "2024")}
#'
#' @param indets Logical, if \code{FALSE}, removes all
#' indeterminate specimens that are not identified to the species level
#' (i.e., records identified only to family or genus).
#'
#' @param reorder Provide a vector with any of \code{c("herbarium", "taxa", "collector", "area", "year")}
#' to reorder the retrieved records based on the specified columns. By default, the
#' data will be redordered according to this vector, meaning the returned dataset
#' will be specifically reordered based on the columns \code{'herbarium'}, \code{'family'},
#' \code{'genus'}, \code{'specificEpithet'}, \code{'recordedBy'}, \code{'recordNumber'},
#' \code{'country'}, \code{'stateProvince'}, \code{'municipality'} and \code{'year'}.
#' You can modify the order of the vector or provide a subset of these columns to
#' customize the reordering of the data accordingly.
#'
#' @param path Optional; a pathway to the computer's directory, where the REFLORA-downloaded
#' dwca folders are. If you do not provide a path, the function will download the
#' most updated version of the REFLORA dwca files.
#'
#' @param updates Logical, if \code{FALSE}, the search will not check for the
#' most updated version of the REFLORA dwca files. This argument is often used if
#' you have defined a specific path to previously downloaded REFLORA dwca files
#' either manually or with function \code{reflora_download}.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when
#' summarizing specimen records will not be printed in the console in full.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk
#'
#' @param dir Pathway to the computer's directory, where the table-formatted
#' summary will be saved. The default is to create a directory named
#'  \code{reflora_records}.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled \code{reflora_records_search.csv}.
#'
#' @return A \code{data.frame} containing occurrence records for the selected taxon
#' and criteria from the chosen REFLORA herbaria. If \code{save = TRUE}, the function
#' will write the results to a CSV file inside the \code{dir} directory, and also
#' generate or append a \code{log.txt} file that summarizes the download session
#' including total records and breakdowns by herbarium, family, genus, country,
#' and state.
#'
#' @seealso \code{\link{reflora_download}}
#' @seealso \code{\link{reflora_parse}}
#'
#' @examples
#' \dontrun{
#'
#' fam_taxa <- c("Fabaceae", "Ochnaceae")
#' reflora_records(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                 taxon = fam_taxa,
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_records",
#'                 filename = "reflora_records_search")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv capture.output
#' @importFrom dplyr bind_rows arrange across
#' @importFrom tidyselect all_of
#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_trans_general
#'
#' @export
#'

reflora_records <- function(herbarium = NULL,
                            repatriated = TRUE,
                            taxon = NULL,
                            state = NULL,
                            recordYear = NULL,
                            indets = TRUE,
                            reorder = NULL,
                            path = NULL,
                            updates = TRUE,
                            verbose = TRUE,
                            save = TRUE,
                            dir = "reflora_records",
                            filename = "reflora_records_search") {

  # herbarium check
  if (!is.null(herbarium)) {
    .arg_check_herbarium(herbarium, verbose = FALSE)
  }

  # state check
  if (!is.null(state)) {
    if (verbose) {
      message("Checking whether the input state list include valid Brazilian state...")
    }
    state <- .arg_check_state(state)
  }

  # recordYear check
  if (!is.null(recordYear)) {
    if (verbose) {
      message("Checking whether the input recordYear range exists in the REFLORA...")
    }
    .arg_check_recordYear(recordYear)
  }

  # dir check
  dir <- .arg_check_dir(dir)

  # Create a new directory to save the dataframe
  # If there is no directory create one in the working directory
  if (!dir.exists(dir)) {
    if (verbose) {
      message(paste0("Creating directory '", dir, "' in working directory..."))
    }
    dir.create(dir)
  }

  if (!is.null(path)) {
    if (updates) {
      if (verbose) {
        message(paste0("Updating dwca files within '", path, "'"))
      }

      # The reflora_download will get updated dwca files only if any of the current
      # versions differ from the REFLORA IPT
      reflora_download(herbarium = herbarium,
                       repatriated = repatriated,
                       verbose = verbose,
                       dir = path)
    }

    # Parse REFLORA dwca files
    dwca_files <- reflora_parse(path = path,
                                herbarium = herbarium,
                                repatriated = repatriated,
                                verbose = verbose)
  } else {

    # The reflora_download will get updated dwca files only if any of the current
    # versions differ from the REFLORA IPT
    reflora_download(herbarium = herbarium,
                     repatriated = repatriated,
                     verbose = verbose,
                     dir = "reflora_download")

    # Parse REFLORA dwca files
    dwca_files <- reflora_parse(path = "reflora_download",
                                herbarium = herbarium,
                                repatriated = repatriated,
                                verbose = verbose)
  }

  # Extract each "occurrence.txt" data frame and merge them
  occur_df <- .merge_occur_txt(dwca_files)

  # Filter occurrence data
  occur_df <- .filter_occur_df(occur_df, taxon, state, recordYear, verbose)

  # Remove indeterminate specimens
  if (indets == FALSE) {
    indets <- c("FAMILY", "GENUS", "SUBFAMILY", "TRIBE", "DIVISION", "ORDER", "CLASS")
    tf <- !occur_df$taxonRank %in% indets
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }

  # Reorder the data by the order of specific columns
  occur_df <- .reorder_df(occur_df, reorder)

  # Remove columns that are completely NA
  occur_df <- occur_df[ , colSums(!is.na(occur_df)) > 0]

  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = occur_df,
              verbose = verbose,
              filename = filename,
              dir = dir)
    .save_log(df = occur_df,
              herbarium = herbarium,
              filename = filename,
              dir = dir)
  }

  return(occur_df)
}
