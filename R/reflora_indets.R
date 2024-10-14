#' Retrieve specific taxon from the downloaded REFLORA collections
#'
#' @author Carlos Calderón & Domingos Cardoso
#'
#' @description Retrieve specific taxon from the Reflora virtual Herbarium at
#' \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_indets(taxon = NULL,
#'                    herbarium = NULL,
#'                    state = NULL,
#'                    path = NULL,
#'                    updates = TRUE,
#'                    verbose = TRUE,
#'                    save = TRUE,
#'                    dir = "reflora_ocurrence",
#'                    filename = "reflora_ocurrence_search")
#'
#' @param taxon A vector with the required taxon.
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records
#' for all REFLORA-hosted herbaria.
#'
#' @param state A vector with the the required Brazilian states.
#'
#' @param level A vector with the taxonomic level as \code{FAMILY} or \code{GENUS}
#' or both.
#'
#' @param recordYear A vector with the required record year or year range. For example,
#' \code{"1992"} or \code{c("1992", "2024")}
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
#'  \code{reflora_ocurrence}.
#'
#' @param filename Name of the output file to be saved. The default is to create
#' a file entitled \code{reflora_ocurrence_search.csv}.
#'
#' @return A dataframe with the information of the chosen taxon from the chosen
#' REFLORA Herbaria.
#'
#' @seealso \code{\link{reflora_download}}
#' @seealso \code{\link{reflora_parse}}
#'
#' @examples
#' \dontrun{
#'
#' fam_taxa <- c("Fabaceae", "Ochnaceae")
#' reflora_indets(taxon = fam_taxa,
#'                    verbose = TRUE,
#'                    save = TRUE,
#'                    dir = "reflora_ocurrence",
#'                    filename = "reflora_ocurrence_search")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom dplyr bind_rows filter
#' @importFrom magrittr "%>%"
#'

reflora_indets <- function(taxon = NULL,
                           herbarium = NULL,
                           state = NULL,
                           level = NULL,
                           path = NULL,
                           updates = TRUE,
                           verbose = TRUE,
                           save = TRUE,
                           dir = "reflora_occurrence",
                           filename = "reflora_occurrence_search") {


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

  #_____________________________________________________________________________
  # Filter by taxon only FILTRAR OS NAs

  if (!is.null(taxon)) {

    tf_fam <- grepl("aceae$", taxon)
    if (any(tf_fam)) {
      occur_df <- occur_df %>%
        dplyr::filter(family %in% taxon)
    }

    tf_gen <- grepl("^[^ ]+$", taxon) & !grepl("aceae$", taxon)
    if (any(tf_gen)) {
      occur_df <- occur_df %>%
        dplyr::filter(genus %in% taxon)
    }

  }


  #_____________________________________________________________________________
  # Filter by state only

  if (!is.null(state)) {
    doccur_df <- occur_df %>%
      dplyr::filter(stateProvince %in% state)
  }


  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = occur_df,
              verbose = verbose,
              filename = filename,
              dir = dir)
  }

  return(occur_df)
}
