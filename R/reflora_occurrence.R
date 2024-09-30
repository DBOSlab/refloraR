#' Retrieve specific taxon from the downloaded REFLORA collections
#'
#' @author Domingos Cardoso & Carlos Calderón
#'
#' @description Retrieve specific taxon from the Reflora virtual Herbarium at
#' \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_record(herbarium = NULL,
#'                taxon = NULL,
#'                state = NULL,
#'                path = NULL,
#'                updates = TRUE,
#'                verbose = TRUE,
#'                save = TRUE,
#'                dir = "reflora_ocurrence",
#'                filename = "reflora_ocurrence_search")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records
#' for all REFLORA-hosted herbaria.
#'
#' @param taxon A vector with the required taxon.
#'
#' @param state A vector with the the required Brazilian states
#'
#' @param path Pathway to the computer's directory, where the REFLORA-downloaded
#' dwca folders are.
#'
#' @param updates Logical, if \code{FALSE}, the search will not check for the
#' most updated version of the REFLORA dwca files.
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
#' reflora_ocurrence(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                  taxon = fam_taxa,
#'                  verbose = TRUE,
#'                  save = TRUE,
#'                  dir = "reflora_ocurrence",
#'                  filename = "reflora_ocurrence_search")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom dplyr filter bind_rows
#' @importFrom magrittr "%>%"
#'

reflora_ocurrence <- function(herbarium = NULL,
                              taxon = NULL,
                              state = NULL,
                              path = NULL,
                              updates = TRUE,
                              verbose = TRUE,
                              save = TRUE,
                              dir = "reflora_ocurrence",
                              filename = "reflora_ocurrence_search") {

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
  merged_occurrence_df <- dplyr::bind_rows(lapply(dwca_files,
                                                  function(x) x[["data"]][["occurrence.txt"]]))

  #_____________________________________________________________________________
  # Filter by taxon only

  if (!is.null(taxon)) {

    tf_fam <- grepl("aceae$", taxon)
    if (any(tf_fam)) {
      df_filtered <- merged_occurrence_df %>%  dplyr::filter(family %in% taxon)
    }

    tf_gen <- grepl("^[^ ]+$", taxon) & !grepl("aceae$", taxon)
    if (any(tf_gen)) {
      df_filtered <- merged_occurrence_df %>% dplyr::filter(genus %in% taxon)
    }

    tf_spp <- grepl("\\s", taxon)
    if (any(tf_spp)) {
      df_filtered <- merged_occurrence_df %>% dplyr::filter(grepl(paste0(taxon, collapse = "|"),
                                                                  taxonName))
    }

  }

  #_____________________________________________________________________________
  # Filter by state only





  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = df_filtered,
              verbose = verbose,
              filename = filename,
              dir = dir)
  }

  return(df_filtered)
}


