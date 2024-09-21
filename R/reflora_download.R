#' Download plant specimen records from REFLORA Virtual Herbarium
#'
#' @author Domingos Cardoso
#'
#' @description Download plant specimen records in Darwin Core Format from any
#' herbarium at [Herbário Virtual REFLORA](https://ipt.jbrj.gov.br/reflora)
#' hosted by the [Rio de Janeiro Botanical Garden](https://www.gov.br/jbrj).
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
#' The default is to create a directory named **reflora_download**
#' and the search results will be saved within a subfolder named by collection
#' code.
#'
#' @return Folder with DwC-A files for an specific or all REFLORA-associated herbaria.
#'
#' @seealso \code{\link{reflora_summary}}
#'
#' @examples
#' \dontrun{
#'
#' reflora_download(herbarium = c("ALCB", "HUEFS", "K", "RB", "US"),
#'                  verbose = TRUE,
#'                  dir = "downloaded_reflora")
#'}
#'
#' @importFrom stringr str_split
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

  ipt_metadata <- readLines("https://ipt.jbrj.gov.br/reflora/dcat",
                            encoding = "UTF-8",
                            warn = F)

  pos = which(grepl("dcat[:]downloadURL\\s", ipt_metadata))
  URLs <- gsub(".*\\s[<]|[>]\\s;$", "", ipt_metadata[pos])
  herb_URLs <- gsub(".*r[=]|[>]\\s;$", "", URLs)
  herb_code <- toupper(gsub("_.*", "", herb_URLs))

  ini = which(grepl("a dcat:Dataset ;", ipt_metadata))
  end = which(grepl("dcat:mediaType \"application/zip\" ;", ipt_metadata))
  temp <- list()
  for (i in seq_along(ini)) {
    temp[[i]] = paste0(ipt_metadata[ini[i]:end[i]], collapse = " | ")
  }
  ipt_metadata = temp

  ipt_metadata <- lapply(ipt_metadata, function(x) strsplit(x, "\\s[|]\\s")[[1]])

  if (!is.null(herbarium)) {
    ipt_metadata <- ipt_metadata[herb_code %in% herbarium]
    URLs <- URLs[herb_code %in% herbarium]
    herb_URLs <- herb_URLs[herb_code %in% herbarium]
    herb_code <- herb_code[herb_code %in% herbarium]
  }

  for (i in seq_along(herb_URLs)) {

    herb_url <- paste0("https://ipt.jbrj.gov.br/reflora/resource?r=", herb_URLs[i])
    getVersion <- readLines(herb_url,
                            encoding = "UTF-8",
                            warn = F)

    ini = which(grepl("latestVersion", getVersion))[1]
    end = which(grepl("None provided", getVersion))[1]

    getVersion = getVersion[ini:end]

    getVersion <- gsub("(\\s){2,}|\\'|,$", "", getVersion)
    getVersion <- gsub(".*[>]", "", getVersion)

    contact <- ipt_metadata[[i]][which(grepl("dcat:contactPoint", ipt_metadata[[i]]))[1]]
    # Regular expression for extracting the name
    name_pattern <- 'vcard:fn "([^"]+)"'
    name <- regmatches(contact, gregexpr(name_pattern, contact, perl = TRUE))[[1]]
    name <- gsub('vcard:fn "|\"', "", name)  # Remove the 'vcard:fn "' part

    # Regular expression for extracting the email
    email_pattern <- '<mailto:([^>]+)>'
    email <- regmatches(contact, gregexpr(email_pattern, contact, perl = TRUE))[[1]]
    email <- gsub('<mailto:|>', "", email)  # Remove the '<mailto:' part

    collection_name <- gsub("^dct:title\\s\"|\\s-\\sHerbário Virtual.*", "", ipt_metadata[[i]][2])
    collection_name <- gsub(".*\\s-\\s", "", collection_name)

    summary_df <- data.frame(collectionCode = herb_code[i],
                             collectionName = collection_name,
                             contactPoint = name[1],
                             hasEmail = email[1],
                             Version = getVersion[1],
                             Published.on = getVersion[2],
                             Records = getVersion[3],
                             Reflora_URL = herb_url)


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

      download.file(url = dwca_file,
                    destfile = destdirfile,
                    method = "curl")

      unzip(destdirfile, exdir = ex_dwca_folder)
      unlink(destdirfile)

      write.csv(summary_df, paste0(ex_dwca_folder, "/", herb_code[i], "_Reflora_version.csv"),
                row.names = FALSE)

      if (verbose) {
        message(paste0(herb_code[i], " collection sucessfully downloaded!"))
      }

    }

  }

}


