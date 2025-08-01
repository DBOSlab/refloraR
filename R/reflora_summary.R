#' Summarize current plant specimen records at REFLORA Virtual Herbarium
#'
#' @author Domingos Cardoso
#'
#' @description Summarize current available plant specimen records at
#' \href{https://ipt.jbrj.gov.br/reflora/}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_summary(herbarium = NULL,
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_summary")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records for all
#' REFLORA-hosted herbaria.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when
#' summarizing specimen records will not be printed in the console in full.
#'
#' @param save Logical, if \code{TRUE}, the search results will be saved on disk.
#'
#' @param dir Pathway to the computer's directory, where the table-formatted
#' summary will be saved. The default is to create a directory named
#'  \code{reflora_summary}.
#'
#' @return A dataframe summarizing current available plant specimen records in
#' REFLORA Virtual Herbarium.
#'
#' @seealso \code{\link{reflora_download}}
#'
#' @examples
#' \dontrun{
#'
#' reflora_summary(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_summary")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#'
#' @export
#'

reflora_summary <- function(herbarium = NULL,
                            verbose = TRUE,
                            save = TRUE,
                            dir = "reflora_summary") {

  # herbarium check
  if (!is.null(herbarium)) {
    if (verbose) {
      message("Checking whether the input herbarium code exist in the REFLORA...")
    }
    .arg_check_herbarium(herbarium)
  }

  # dir check
  dir <- .arg_check_dir(dir)

  # Get raw metadata from REFLORA repository
  ipt_info <- .get_ipt_info(herbarium)
  ipt_metadata = ipt_info[[1]]
  herb_URLs = ipt_info[[2]]
  herb_code = ipt_info[[3]]

  summary_df <- data.frame(collectionCode = herb_code,
                           rightsHolder = NA,
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

    herb_info <- .get_herb_info(herb_URLs, ipt_metadata, i)

    tf <- summary_df$collectionCode %in% herb_code[i]

    summary_df$rightsHolder[tf] <- herb_info[[4]][1]
    summary_df$contactPoint[tf] <- herb_info[[2]][1]
    summary_df$hasEmail[tf] <- herb_info[[3]][1]

    summary_df$Repatriated[tf] <- herb_info[[6]][1]
    summary_df$Version[tf] <- herb_info[[1]][1]
    summary_df$Published.on[tf] <- herb_info[[1]][2]
    summary_df$Records[tf] <- herb_info[[1]][3]
    summary_df$Reflora_URL[tf] <- herb_info[[5]]

  }

  summary_df$Records <- as.numeric(gsub(",", "", summary_df$Records))
  summary_df <- summary_df[order(summary_df$collectionCode), ]
  row.names(summary_df) <- 1:length(row.names(summary_df))

  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = summary_df,
              verbose = verbose,
              filename = "reflora_summary",
              dir = dir)
  }

  return(summary_df)

}

