#' Download plant specimen records from Reflora Virtual Herbarium
#'
#' @author
#' Domingos Cardoso
#'
#' @description
#' Download plant specimen records in Darwin Core Format from any
#' herbarium collection at \href{https://ipt.jbrj.gov.br/reflora/}{Reflora Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#' Reflora aggregates collections from both Brazilian and international herbaria
#' that hold Brazilian specimens.
#'
#' @usage
#' reflora_download(herbarium = NULL,
#'                  repatriated = TRUE,
#'                  verbose = TRUE,
#'                  dir = "reflora_download")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as NULL to download records for all Reflora herbaria.
#'
#' @param repatriated Logical. If \code{FALSE}, skips downloading records from
#' Reflora-associated herbaria that have been repatriated. Default is \code{TRUE}.
#' Use \code{reflora_summary()} to check which collections are repatriated.
#' In this context, “digital repatriation” refers to making high-resolution images
#' and associated specimen metadata openly accessible through a Brazilian public
#' infrastructure (HVR/IPT).
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when downloading
#' herbarium specimen records will not be printed in the console in full.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved.
#' The default is to create a directory named \code{reflora_download}
#' and the results will be saved within a subfolder named by each searched
#' Reflora-associated herbarium collection.
#'
#' @return Invisibly returns \code{NULL}. This function is called for downloading
#' and extracting Darwin Core Archive (DwC-A) files into the specified directory.
#'
#' @seealso \code{\link{reflora_summary}}
#'
#' @examples
#' \dontrun{
#'
#' reflora_download(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                  verbose = TRUE,
#'                  dir = "reflora_download")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils download.file unzip write.csv
#'
#' @export
#'

reflora_download <- function(herbarium = NULL,
                             repatriated = TRUE,
                             verbose = TRUE,
                             dir = "reflora_download") {

  # herbarium check
  if (!is.null(herbarium)) {
    .arg_check_herbarium(herbarium, verbose = verbose)
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

  # Get raw metadata from Reflora repository
  reflora_summary_ipt <- reflora_summary(herbarium,
                                         records = "none",
                                         verbose = FALSE,
                                         save = FALSE)

  herb_URLs = reflora_summary_ipt$resourceKey
  herb_code = reflora_summary_ipt$collectionCode

  for (i in seq_along(herb_URLs)) {

    summary_df <- data.frame(collectionCode = herb_code[i],
                             rightsHolder = reflora_summary_ipt$rightsHolder[i],
                             Repatriated = reflora_summary_ipt$Repatriated[i],
                             contactPoint = reflora_summary_ipt$contactPoint[i],
                             hasEmail = reflora_summary_ipt$hasEmail[i],
                             Version = reflora_summary_ipt$Version[i],
                             Published.on = reflora_summary_ipt$Published.on[i],
                             Records = NA,
                             Reflora_URL = reflora_summary_ipt$Reflora_URL[i])

    # Do not download repatriated collections if repatriated = FALSE
    if (!repatriated && isTRUE(summary_df$Repatriated)) {
      if (verbose) message(paste0("Skipping repatriated collection: ", summary_df$collectionCode))
      next
    }

    vdest = gsub("[.].*", "", summary_df$Version)
    vlast = gsub(".*[.]", "", summary_df$Version)

    dwca_folder = paste0("dwca-", herb_URLs[i], "-v", vdest)

    ex_dwca_folder = paste0("dwca-", herb_URLs[i], "-v", vdest, "_", vlast)
    reflora_database = paste0(dir, "/", gsub("-", "_", ex_dwca_folder))

    if (!dir.exists(reflora_database) |
        !any(list.files(reflora_database) %in% "occurrence.txt")) {

      tf <- grepl(paste0("dwca_", herb_URLs[i]), list.files(dir))
      if (any(tf)) {
        unlink(paste0(dir,"/", list.files(dir)[tf]), recursive = TRUE)
      }

      dwca_file = paste0("https://ipt.jbrj.gov.br/reflora/archive.do?r=",
                         herb_URLs[i],
                         "&v=",
                         summary_df$Version)

      destdirfile = paste0(dir, "/", dwca_folder)
      ex_dwca_folder <- paste0(dir, "/", gsub("-", "_", ex_dwca_folder))

      if (verbose) {
        message(paste0("Downloading DwC-A files for the collection... ",
                       herb_code[i], " ", i, "/",
                       length(herb_code)))
      }

      utils::download.file(url = dwca_file,
                           destfile = destdirfile,
                           method = "curl")

      utils::unzip(destdirfile, exdir = ex_dwca_folder)
      unlink(destdirfile)

      utils::write.csv(summary_df, paste0(ex_dwca_folder, "/", herb_code[i], "_Reflora.csv"),
                       row.names = FALSE)

      if (verbose) {
        message(paste0(herb_code[i], " collection sucessfully downloaded!"))
      }

    }

  }

}
