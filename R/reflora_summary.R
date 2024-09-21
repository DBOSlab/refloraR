#' Summarize current plant specimen records at REFLORA Virtual Herbarium
#'
#' @author Domingos Cardoso
#'
#' @description Summarize current available plant specimen records at
#' [Herbário Virtual REFLORA](https://ipt.jbrj.gov.br/reflora)
#' hosted by the [Rio de Janeiro Botanical Garden](https://www.gov.br/jbrj).
#'
#' @usage
#' reflora_summary(herbarium = NULL,
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_summary")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as NULL to summarize specimen records for all
#' REFLORA-hosted herbaria.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when
#' summarizing specimen records will not be printed in the console in full.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the table-formatted
#' summary will be saved. The default is to create a directory named
#' **reflora_summary**.
#'
#' @return A dataframe summarizing current available plant specimen records in
#' REFLORA Virtual Herbarium.
#'
#' @seealso \code{\link{reflora_download}}
#'
#' @examples
#' \dontrun{
#'
#' reflora_summary(herbarium = c("ALCB", "HUEFS", "K", "RB", "US"),
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_summary")
#'}
#'
#' @importFrom stringr str_split
#'
#' @export
#'

reflora_summary <- function(herbarium = NULL,
                            verbose = TRUE,
                            save = TRUE,
                            dir = "reflora_summary") {

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

  repatriated <- c("US", "NY", "MO", "GH", "S", "P", "K", "W")

  summary_df <- data.frame(collectionCode = herb_code,
                           collectionName = NA,
                           Repatriated = NA,
                           contactPoint = NA,
                           hasEmail = NA,
                           Version = NA,
                           Published.on = NA,
                           Records = NA,
                           Reflora_URL = NA)

  for (i in seq_along(herb_URLs)) {

    if (verbose) {
      message(paste0("Summarizing specimen collections of ",
                     herb_code[i], " ", i, "/",
                     length(herb_code)))
    }

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

    tf <- summary_df$collectionCode %in% herb_code[i]

    summary_df$collectionName[tf] <- collection_name
    summary_df$contactPoint[tf] <- name[1]
    summary_df$hasEmail[tf] <- email[1]

    summary_df$Repatriated[tf] <- herb_code[i] %in% repatriated
    summary_df$Version[tf] <- getVersion[1]
    summary_df$Published.on[tf] <- getVersion[2]
    summary_df$Records[tf] <- getVersion[3]
    summary_df$Reflora_URL[tf] <- herb_url

  }

  summary_df$Records <- as.numeric(gsub(",", "", summary_df$Records))
  summary_df <- summary_df[order(summary_df$collectionCode), ]
  row.names(summary_df) <- 1:length(row.names(summary_df))

  # Save the search results if param save is TRUE
  if (save) {
  saveCSV(df = summary_df,
          verbose = verbose,
          filename = "reflora_summary.csv",
          dir = dir)
  }

  return(summary_df)

}

