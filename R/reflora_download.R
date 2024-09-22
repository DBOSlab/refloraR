#' Download plant specimen records from REFLORA Virtual Herbarium
#'
#' @author Domingos Cardoso
#'
#' @description Download plant specimen records in Darwin Core Format from any
#' herbarium collection at \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_download(herbarium = NULL,
#'                  verbose = TRUE,
#'                  dir = "reflora_download")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as NULL to download records for all REFLORA herbaria.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when downloading
#' herbarium specimen records will not be printed in the console in full.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved.
#' The default is to create a directory named \code{reflora_download}
#' and the results will be saved within a subfolder named by each searched
#' REFLORA-associated herbarium collection.
#'
#' @return Folder with DwC-A files for an specific or all REFLORA-associated herbaria.
#'
#' @seealso \code{\link{reflora_summary}}
#'
#' @examples
#' \dontrun{
#'
#' reflora_download(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                  verbose = TRUE,
#'                  dir = "downloaded_reflora")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils download.file unzip write.csv
#'
#' @export
#'

reflora_download <- function(herbarium = NULL,
                             verbose = TRUE,
                             dir = "reflora_download") {


  # Create a new directory to save the results.
  # If there is no directory... make one!
  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  # Get raw metadata from REFLORA repository
  ipt_info <- .get_ipt_info(herbarium)
  ipt_metadata = ipt_info[[1]]
  herb_URLs = ipt_info[[2]]
  herb_code = ipt_info[[3]]

  for (i in seq_along(herb_URLs)) {

    herb_info <- .get_herb_info(herb_URLs, ipt_metadata, i)

    summary_df <- data.frame(collectionCode = herb_code[i],
                             rightsHolder = herb_info[[4]][1],
                             contactPoint = herb_info[[2]][1],
                             hasEmail = herb_info[[3]][1],
                             Version = herb_info[[1]][1],
                             Published.on = herb_info[[1]][2],
                             Records = herb_info[[1]][3],
                             Reflora_URL = herb_info[[5]])

    vdest = gsub("[.].*", "", summary_df$Version)
    vlast = gsub(".*[.]", "", summary_df$Version)

    dwca_folder = paste0("dwca-", herb_URLs[i], "-v", vdest)

    ex_dwca_folder = paste0("dwca-", herb_URLs[i], "-v", vdest, "_", vlast)
    ffb_database = paste0(dir, "/", gsub("-", "_", ex_dwca_folder))

    if (!dir.exists(ffb_database)) {

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

      utils::write.csv(summary_df, paste0(ex_dwca_folder, "/", herb_code[i], "_Reflora_version.csv"),
                       row.names = FALSE)

      if (verbose) {
        message(paste0(herb_code[i], " collection sucessfully downloaded!"))
      }

    }

  }

}


