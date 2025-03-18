#' Retrieve specific taxon from the downloaded REFLORA collections
#'
#' @author Carlos Calderón & Domingos Cardoso
#'
#' @description Retrieve specific taxon from the Reflora virtual Herbarium at
#' \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_occurrence(herbarium = NULL,
#'                    taxon = NULL,
#'                    state = NULL,
#'                    recordYear = NULL,
#'                    reorder = c("herbarium", "taxa", "collector", "area", "year")
#'                    path = NULL,
#'                    updates = TRUE,
#'                    verbose = TRUE,
#'                    save = TRUE,
#'                    dir = "reflora_ocurrence",
#'                    filename = "reflora_ocurrence_search")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records
#' for all REFLORA-hosted herbaria.
#'
#' @param taxon A vector with the required taxon. It can be one or a vector of
#' multiple scientific names at family, genus or species level.
#'
#' @param state A vector with the required Brazilian state(s) (full name or acronym).
#'
#' @param recordYear A vector with the required record year or year range. For example,
#' \code{"1992"} or \code{c("1992", "2024")}
#'
#' @param reorder Provide a vector with any of \code{c("herbarium", "taxa", "collector", "area", "year")}
#' to reorder the retrieved records based on the specified columns. By default, the
#' data will be redordered according to this vector, meaning the returned dataset
#' will be specifcially reordered based on the columns \code{'herbarium'}, \code{'family'},
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
#' reflora_occurrence(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                    taxon = fam_taxa,
#'                    verbose = TRUE,
#'                    save = TRUE,
#'                    dir = "reflora_ocurrence",
#'                    filename = "reflora_ocurrence_search")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom dplyr bind_rows arrange across
#' @importFrom tidyselect all_of
#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_trans_general
#'
#' @export
#'



reflora_occurrence <- function(herbarium = NULL,
                               taxon = NULL,
                               state = NULL,
                               recordYear = NULL,
                               reorder = c("herbarium", "taxa", "collector", "area", "year"),
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
  if (!is.null(state)) {
    state <- .arg_check_state(state)
  }

  # recordYear check
  .arg_check_recordYear(recordYear)

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

  # Apply function to convert 'recordNumber' to character in all dataframes inside `dwca_files`
  dwca_files <- lapply(dwca_files, function(x) {
    x[["data"]][["occurrence.txt"]][["recordNumber"]] <- as.character(x[["data"]][["occurrence.txt"]][["recordNumber"]])
    return(x)
  })

  # Extract each "occurrence.txt" data frame and merge them
  occur_df <- dplyr::bind_rows(lapply(dwca_files,
                                      function(x) x[["data"]][["occurrence.txt"]]))

  temp_occur_df <- data.frame(matrix(ncol = length(names(occur_df)), nrow = 0))
  colnames(temp_occur_df) <- names(occur_df)

  #_____________________________________________________________________________
  # Filter by taxon only

  if (!is.null(taxon)) {
    if (verbose) {
      message("\nFiltering taxon names... ")
    }

    tf_fam <- grepl("aceae$", taxon)
    if (any(tf_fam)) {
      taxon_fam <- taxon[tf_fam]
      if (any(tf)) {
        occur_df_fam <- occur_df[tf, ]
        temp_occur_df <- occur_df_fam
      }
    }

    tf_gen <- grepl("^[^ ]+$", taxon) & !grepl("aceae$", taxon)
    if (any(tf_gen)) {
      taxon_gen <- taxon[tf_gen]
      if (any(tf)) {
        occur_df_gen <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_gen)
      }
    }

    tf_spp <- grepl("\\s", taxon)
    if (any(tf_spp)) {
      taxon_spp <- taxon[tf_spp]
      tf <- occur_df$taxonName %in% taxon_spp
      if (any(tf)) {
        occur_df_spp <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_spp)
      }
    }

    if (nrow(temp_occur_df) != 0){
      occur_df <- temp_occur_df
    }

  }


  #_____________________________________________________________________________
  # Filter by state only

  if (!is.null(state)) {
    if (verbose) {
      message("\nFiltering states... ")
    }
    tf <- occur_df$stateProvince %in% state
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }


  #_____________________________________________________________________________
  # Filter by record year only

  if (!is.null(recordYear)) {
    if (verbose) {
      message("\nFiltering recordYear... ")
    }
    if (length(recordYear) == 1) {
      # If only one year is given, filter for that specific year
      occur_df <- occur_df[occur_df$year == recordYear, ]
    } else if (length(recordYear) == 2) {
      # If a range is given, filter for records within that range (inclusive)
      occur_df <- occur_df[occur_df$year >= recordYear[1] & occur_df$year <= recordYear[2], ]
    }
  }


  #_____________________________________________________________________________
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
