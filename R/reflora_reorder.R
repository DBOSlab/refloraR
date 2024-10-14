#' Reorder the downloaded REFLORA occurrence data
#'
#' @author Domingos Cardoso & Carlos Calderón
#'
#' @description Retrieve specific taxon from the Reflora virtual Herbarium at
#' \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_reorder(df = NULL,
#'                 reorder = c("taxa", "collector", "area", "year"),
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_reorder",
#'                 filename = "reflora_ocurrence_reordered")
#'
#' @param df A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records
#' for all REFLORA-hosted herbaria.
#'
#' @param reorder A vector with order to be considered:
#'  c("taxa", "collector", "area", "year").
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
#' reflora_reorder(df = occur_df,
#'                 reorder = c("taxa", "collector", "area", "year"),
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_reorder",
#'                 filename = "reflora_ocurrence_reordered")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom dplyr bind_rows filter
#' @importFrom magrittr "%>%"
#'
#' @export
#'

reflora_reorder <- function(df = NULL,
                            reorder = c("taxa", "collector", "area", "year"),
                            verbose = TRUE,
                            save = TRUE,
                            dir = "reflora_reorder",
                            filename = "reflora_ocurrence_reordered") {

  # dir check
  dir <- .arg_check_dir(dir)

  # Create a new directory to save the dataframe
  # If there is no directory create one in the working directory
  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  #





  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = df,
              verbose = verbose,
              filename = filename,
              dir = dir)
  }

  return(df)
}


