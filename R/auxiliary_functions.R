# Auxiliary functions to support main functions
# Author: Domingos Cardoso

#_______________________________________________________________________________
### Function to save csv files ###

.save_csv <- function(df,
                      verbose = TRUE,
                      filename = NULL,
                      dir = dir) {

  # Save the data frame if param save is TRUE
  # Create a new directory to save the results with current date
  # If there is no directory... make one!

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  filename <- paste0(filename, ".csv")
  # Create and save the spreadsheet in .csv format
  if (verbose) {
    message(paste0("Writing spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  utils::write.csv(df, file = paste0(dir, "/", filename), row.names = FALSE)
}


#_______________________________________________________________________________
### Function to get raw metadata from REFLORA repository ###

.get_ipt_info <- function(herbarium) {

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
    #URLs <- URLs[herb_code %in% herbarium]
    herb_URLs <- herb_URLs[herb_code %in% herbarium]
    herb_code <- herb_code[herb_code %in% herbarium]
  }
  return(list(ipt_metadata, herb_URLs, herb_code))
}


#_______________________________________________________________________________
### Function to get summary information of each REFLORA-associated collection ###

.get_herb_info <- function(herb_URLs, ipt_metadata, i) {

  herb_url <- paste0("https://ipt.jbrj.gov.br/reflora/resource?r=", herb_URLs[i])
  version <- readLines(herb_url,
                       encoding = "UTF-8",
                       warn = F)

  ini = which(grepl("latestVersion", version))[1]
  end = which(grepl("None provided", version))[1]

  version = version[ini:end]

  version <- gsub("(\\s){2,}|\\'|,$", "", version)
  version <- gsub(".*[>]", "", version)

  contact <- ipt_metadata[[i]][which(grepl("dcat:contactPoint", ipt_metadata[[i]]))[1]]
  # Regular expression for extracting the name
  name_pattern <- 'vcard:fn "([^"]+)"'
  name <- regmatches(contact, gregexpr(name_pattern, contact, perl = TRUE))[[1]]
  name <- gsub('vcard:fn "|\"', "", name)  # Remove the 'vcard:fn "' part

  # Regular expression for extracting the email
  email_pattern <- '<mailto:([^>]+)>'
  email <- regmatches(contact, gregexpr(email_pattern, contact, perl = TRUE))[[1]]
  email <- gsub('<mailto:|>', "", email)  # Remove the '<mailto:' part

  rights_holder <- gsub("^dct:title\\s\"|\\s-\\sHerb\u00E1rio Virtual.*",
                        "", ipt_metadata[[i]][2])
  rights_holder <- gsub("-\\sAmostras\\sBrasileiras.*",
                        "", rights_holder)
  rights_holder <- gsub(".*\\s-\\s|^\\s|\\s$|.*(H|h)erbarium-\\s|.*Herb\u00E1rio\\s(da|do)\\s|[.]\\sHerb\u00E1rio\\sVirtual\\s.*",
                        "", rights_holder)

  return(list(version, name, email, rights_holder, herb_url))
}


#_______________________________________________________________________________
### Function to reorder retrieved data based on specific columns ###

.reorder_df <- function(df, reorder) {

  if (!is.null(reorder)) {
    columns_to_order <- reorder
  } else {
    columns_to_order <- c("herbarium", "taxa", "collector", "area", "year")
  }

  tf <- columns_to_order %in% "herbarium"
  if (any(tf)) {
    columns_to_order[tf] <- "collectionCode"
  }
  tf <- columns_to_order %in% "taxa"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("family", "genus", "specificEpithet"),
                                      columns_to_replace = "taxa")
  }
  tf <- columns_to_order %in% "collector"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("recordedBy", "recordNumber"),
                                      columns_to_replace = "collector")
  }
  tf <- columns_to_order %in% "area"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("country", "stateProvince", "municipality"),
                                      columns_to_replace = "area")
  }

  # Arrange the dataframe based on the vector of column names
  df <- df %>%
    dplyr::arrange(dplyr::across(tidyselect::all_of(columns_to_order)))

  return(df)
}

.replace_cols <- function (columns_to_order,
                           replace,
                           columns_to_replace) {
  index <- which(columns_to_order == columns_to_replace)
  # Perform the replacement based on the index position
  if (index == 1) {
    # Special handling if 'herbarium' is the first element
    columns_to_order <- c(replace, columns_to_order[(index+1):length(columns_to_order)])
  } else if (index == length(columns_to_order)) {
    # Special handling if the element to replace is the last element
    columns_to_order <- c(columns_to_order[1:(index-1)], replace)
  } else {
    # Standard replacement for elements in the middle
    columns_to_order <- c(columns_to_order[1:(index-1)], replace,
                          columns_to_order[(index+1):length(columns_to_order)])
  }

  return(columns_to_order)
}


