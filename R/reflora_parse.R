#' Parse Darwin Core Archive files
#'
#' @author Carlos Calderón and Domingos Cardoso
#'
#' @description Read Darwin Core Archive (DwC-A) files from any downloaded dwca
#' folder at \href{https://ipt.jbrj.gov.br/reflora}{REFLORA Virtual Herbarium}
#' hosted by the \href{https://www.gov.br/jbrj}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' reflora_parse(path = NULL,
#'               verbose = TRUE)
#'
#' @param path Pathway to the computer's directory, where the REFLORA-downloaded
#' dwca folders are.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when
#' summarizing specimen records will not be printed in the console in full.
#'
#' @return A list of dwca files, including data and associated metada.
#'
#' @seealso \code{\link{reflora_download}}
#'
#' @examples
#' \dontrun{
#'
#' reflora_download(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                  verbose = TRUE,
#'                  dir = "reflora_download")
#'
#' dwca <- reflora_parse(path = "reflora_download",
#'                       verbose = TRUE)
#'}
#'
#' @importFrom finch dwca_read
#' @importFrom utils read.csv
#'

reflora_parse <- function(path = NULL,
                          verbose = TRUE) {

  dwca_folders <- list.files(path)
  dwca_filenames <- lapply(paste0(path, "/", dwca_folders), list.files)

  # path check
  .arg_check_path(path, dwca_folders, dwca_filenames)

  # Calling all dwca files
  dwca_files <- lapply(dwca_folders,
                       function(x) finch::dwca_read(input = paste0(path, "/", x),
                                                    read = TRUE,
                                                    encoding = "UTF-8"))

  # Parsing csv files, if they exist
  tf <- lapply(dwca_filenames, function(x) grepl("[.]csv$", x))
  tf_csv <- unlist(lapply(tf, any))

  if (any(tf_csv)) {
    csv_filenames <- unlist(Map(`[`, dwca_filenames, tf)[tf_csv])
    dwca_folders = dwca_folders[tf_csv]
    csv_df <- lapply(seq_along(csv_filenames),
                     function(i) utils::read.csv(paste0(path,
                                                        "/",
                                                        dwca_folders[i],
                                                        "/",
                                                        csv_filenames[i])))

    name <- paste0("summary_",
                   unlist(lapply(seq_along(csv_df),
                                 function(i) csv_df[[i]]$collectionCode)))

    # Use mapply to add each summary data frame to each dwca_files element
    dwca_files[tf_csv] <- mapply(function(x, y, name) {
      x[[name]] <- y
      x
    }, dwca_files[tf_csv], csv_df, name, SIMPLIFY = FALSE)

  }

  # Name the list with uploaded dwca files
  temp <- unlist(lapply(seq_along(dwca_files),
                        function(i) dwca_files[[i]][["files"]][["xml_files"]][1]))
  names(dwca_files) <- gsub(".*[/]", "", gsub("[/]eml.*", "", temp))


  if (verbose) {
    message("The collections and associated metadata were parsed from the following dwca folders: \n\n",
            paste0(names(dwca_files), "\n"))
  }

  return(dwca_files)

}
