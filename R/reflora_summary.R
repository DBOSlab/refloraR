#' Summarize current plant specimen records at REFLORA Virtual Herbarium
#'
#' @author Domingos Cardoso
#'
#' @description Summarize current available plant specimen records at
#' \href{https://ipt.jbrj.gov.br/reflora/}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#' REFLORA aggregates collections from both Brazilian and international herbaria
#' that hold Brazilian specimens. In this context, “digital repatriation” refers
#' to making high-resolution images and associated specimen metadata openly
#' accessible through a Brazilian public infrastructure (HVR/IPT), even when the
#' physical specimens remain curated in the holding herbarium. The
#' \code{reflora_summary()} output includes a repatriation status field to help
#' users identify these digitally repatriated collections.
#'
#' @usage
#' reflora_summary(herbarium = NULL,
#'                 verbose = TRUE,
#'                 save = TRUE,
#'                 dir = "reflora_summary")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to summarize specimen records for
#' all REFLORA-hosted herbaria.
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
#' @return A \code{data.frame} with one row per herbarium collection and the
#' following columns:
#' \describe{
#'   \item{collectionCode}{Character. Herbarium acronym (collection code) as
#'   registered in the REFLORA Virtual Herbarium.}
#'
#'   \item{rightsHolder}{Character. Institution responsible for the collection,
#'   as reported in the IPT metadata.}
#'
#'   \item{Repatriated}{Logical. Indicates whether the herbarium corresponds to
#'   a repatriated collection (i.e., Brazilian specimens digitized from foreign
#'   institutions such as US, NY, K, etc.).}
#'
#'   \item{contactPoint}{Character. Name of the contact person or curator
#'   associated with the collection.}
#'
#'   \item{hasEmail}{Character. Contact email address for the contactPoint,
#'   when available.}
#'
#'   \item{Version}{Character. Current dataset version number available in
#'   the IPT repository.}
#'
#'   \item{Published.on}{Character. Date and hour on which the current dataset
#'    version was published in the IPT.}
#'
#'   \item{Records}{Numeric. Total number of specimen records currently
#'   available for the collection.}
#'
#'   \item{Reflora_URL}{Character. Direct URL to the collection page in the
#'   REFLORA Virtual Herbarium.}
#' }
#'
#' @seealso \code{\link{reflora_download}}
#'
#' @examples
#' \dontrun{
#'
#' reflora_summary(herbarium = c("ALCB", "HUEFS"),
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
    .arg_check_herbarium(herbarium, verbose = verbose)
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
