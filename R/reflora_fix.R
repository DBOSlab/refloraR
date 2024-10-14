#' Fix specific columns from the downloaded REFLORA occurrence data
#'
#' @author Domingos Cardoso & Carlos Calderón
#'
#' @description Retrieve specific taxon from the Reflora virtual Herbarium at
#' \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_fix(df = NULL,
#'             scientificName = TRUE,
#'             recordBy = TRUE,
#'             coordinates = TRUE,
#'             country = TRUE,
#'             verbose = TRUE,
#'             save = TRUE,
#'             dir = "reflora_fix",
#'             filename = "reflora_ocurrence_fixed")
#'
#' @param df A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records
#' for all REFLORA-hosted herbaria.
#'
#' @param scientificName Logical, if \code{FALSE}, the search will not check for the
#' most updated version of the REFLORA dwca files. This argument is often used if
#' you have defined a specific path to previously downloaded REFLORA dwca files
#' either manually or with function \code{reflora_occurrence}.
#'
#' @param recordBy Logical, if \code{FALSE}, the search will not check for the
#' most updated version of the REFLORA dwca files. This argument is often used if
#' you have defined a specific path to previously downloaded REFLORA dwca files
#' either manually or with function \code{reflora_occurrence}.
#'
#' @param coordinates Logical, if \code{FALSE}, the search will not check for the
#' most updated version of the REFLORA dwca files. This argument is often used if
#' you have defined a specific path to previously downloaded REFLORA dwca files
#' either manually or with function \code{reflora_occurrence}.
#'
#' @param country Logical, if \code{FALSE}, the search will not check for the
#' most updated version of the REFLORA dwca files. This argument is often used if
#' you have defined a specific path to previously downloaded REFLORA dwca files
#' either manually or with function \code{reflora_occurrence}.
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
#' a file entitled \code{reflora_ocurrence_fixed.csv}.
#'
#' @return A dataframe with the information of the chosen taxon from the chosen
#' REFLORA Herbaria.
#'
#' @seealso \code{\link{reflora_ocurrence}}
#'
#' @examples
#' \dontrun{
#'
#' fam_taxa <- c("Fabaceae", "Ochnaceae")
#' occur_df <- reflora_occurrence(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                                taxon = fam_taxa,
#'                                verbose = TRUE,
#'                                save = FALSE)
#'
#' reflora_fix(df = occur_df,
#'             scientificName = TRUE,
#'             recordBy = TRUE,
#'             coordinates = TRUE,
#'             country = TRUE,
#'             verbose = TRUE,
#'             save = TRUE,
#'             dir = "reflora_fix",
#'             filename = "reflora_ocurrence_fixed")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom dplyr bind_rows filter
#' @importFrom magrittr "%>%"
#'
#' @export
#'

reflora_fix <- function(df = NULL,
                        scientificName = TRUE,
                        recordBy = TRUE,
                        coordinates = TRUE,
                        country = TRUE,
                        verbose = TRUE,
                        save = TRUE,
                        dir = "reflora_fix",
                        filename = "reflora_ocurrence_fixed") {

  # dir check
  dir <- .arg_check_dir(dir)

  # Create a new directory to save the dataframe
  # If there is no directory create one in the working directory
  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  # Fix scientificName
  if (scientificName) {
    tf <- !is.na(df$scientificNameAuthorship)
    if (any(tf)) {
      df$scientificName[tf] <- paste(df$taxonName[tf],
                                     df$scientificNameAuthorship[tf])
    }
  }

  # Fix recordBy
  if (recordBy) {

    # Underdevelopment with the use Domingos Cardoso's new package herbCur

  }

  # Fix decimalLatitude and decimalLongitude
  if (coordinates) {

    # Underdevelopment with the use Bruno Ribeiro's package bdc
    # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13868
    # https://brunobrr.github.io/bdc/articles/taxonomy.html

  }

  # Fix country
  # Keep all country names in English format
  # And also put as NA when there is no identified country name inside the column
  if (country) {

  }




  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = df,
              verbose = verbose,
              filename = filename,
              dir = dir)
  }

  return(df)
}


