# Small functions to evaluate user input for the main functions and
# return meaningful errors.
# Author: Domingos Cardoso & Carlos Calderón

#_______________________________________________________________________________
# Check if the dir input is "character" type and if it has a "/" in the end
.arg_check_dir <- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"character" %in% class_x) {
    stop(paste0("The argument dir should be a character, not '", class_x, "'."),
         call. = FALSE)
  }
  if (grepl("[/]$", x)) {
    x <- gsub("[/]$", "", x)
  }
  return(x)
}


#_______________________________________________________________________________
# Check path
.arg_check_path <- function(path, dwca_folders, dwca_filenames) {
  # Check classes
  class_path <- class(path)
  if (!"character" %in% class_path) {
    stop(paste0("The argument path should be a character, not '", class_path, "'."),
         call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop(paste0("There is no folder '", path, "' in the working directory."),
         call. = FALSE)
  } else {
    if (!any(grepl("^dwca", dwca_folders))) {
      stop(paste0("There is no REFLORA-downloaded dwca folder within the directory '", path, "'."),
           call. = FALSE)
    } else {
      tf <- 0 == unlist(lapply(dwca_filenames, length))
      if (any(tf)) {
        if (length(which(tf)) == 1) {
          stop(paste0(paste0("The dwca folder ",
                             paste0("'", paste0(dwca_folders[tf]), "'", collapse = ", "),
                             " within the directory '", path, "' is fully empty.\n\n"),
                      "Either download such dwca folder again or exclude it."),
               call. = FALSE)
        } else {
          stop(paste0(paste0("The dwca folders ",
                             paste0("'", paste0(dwca_folders[tf]), "'", collapse = ", "),
                             " within the directory '", path, "' are fully empty.\n\n"),
                      "Either download such dwca folders again or exclude them."),
               call. = FALSE)
        }

      }

      tf <- lapply(dwca_filenames, function(x) c("identification.txt", "occurrence.txt") %in% x)
      tf <- !unlist(lapply(tf, any))

      if (any(tf)) {
        stop(paste0(paste0("An 'identification.txt' and/or 'occurrence.txt' files are missing in the dwca folders ",
                           paste0("'", paste0(dwca_folders[tf]), "'", collapse = ", "), ".\n\n"),
                    "Either download such dwca folders again or exclude them."),
             call. = FALSE)
      }

    }
  }
}


#_______________________________________________________________________________
# Check the recordYear input
.arg_check_recordYear <- function(x) {
  if (length(x) > 2) {
    stop("The argument 'recordYear' should be either a single year or a range of two years.")
  }

  # Check if all elements have exactly four digits
  if (!all(nchar(x) == 4 & grepl("^[0-9]{4}$", x))) {
    stop("All elements must be 4-digit numbers.")
  }

  # If there are two elements, check if the first is less than the second
  if (length(x) == 2 && as.numeric(x[1]) > as.numeric(x[2])) {
    stop("If a range is provided, the first year must be less than the second year.")
  }
}


#_______________________________________________________________________________
# Check the state input
.arg_check_state <- function(x) {
  # Named list of Brazilian states with their acronyms
  valid_states <- c("Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
                    "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
                    "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
                    "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
                    "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
                    "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
                    "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
                    "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
                    "Tocantins" = "TO")

  # Create a lookup table with both full names and acronyms for easy access
  valid_states_full <- names(valid_states)
  valid_states_acronyms <- unname(valid_states)

  # Remove diacritics from both the states list and the input
  states_no_diacritics <- stringi::stri_trans_general(x, "Latin-ASCII")
  valid_states_full_no_diacritics <- stringi::stri_trans_general(valid_states_full, "Latin-ASCII")
  valid_states_acronyms_no_diacritics <- stringi::stri_trans_general(valid_states_acronyms, "Latin-ASCII")

  # Initialize a vector to store the corrected names
  corrected_states <- character(length(x))

  for (i in seq_along(x)) {
    # Check if the state matches a full name without diacritics
    match_full <- match(states_no_diacritics[i], valid_states_full_no_diacritics)
    # Check if the state matches an acronym without diacritics
    match_acronym <- match(states_no_diacritics[i], valid_states_acronyms_no_diacritics)

    if (!is.na(match_full)) {
      # If it matches a full name, return the full name
      corrected_states[i] <- valid_states_full[match_full]
    } else if (!is.na(match_acronym)) {
      # If it matches an acronym, return the corresponding full name
      corrected_states[i] <- names(valid_states)[match_acronym]
    } else {
      # If it doesn't match anything, mark it as invalid
      stop(paste("The following Brazilian state is not valid:", x[i]))
    }
  }

  # Return the corrected state names
  return(corrected_states)
}


#_______________________________________________________________________________
# Check the herbarium input
.arg_check_herbarium <- function(x) {

  # Get raw metadata from REFLORA repository
  ipt_info <- .get_ipt_info(herbarium = NULL)

  # Correct herbarium acronyms
  correct_acronyms <- ipt_info[[3]]

  # Identify any acronyms in the input that are not valid
  invalid_acronyms <- x[!x %in% correct_acronyms]

  if (length(invalid_acronyms) > 0) {

    stop(paste("Acronyms", .format_acronyms(invalid_acronyms),
                " not found in the REFLORA Virtual Herbarium.\n\n"),
         "Use the function 'reflora_summary' to check which are the collections currently available in the REFLORA.")
  }
}

.format_acronyms <- function(acronyms) {
  n <- length(acronyms)
  if (n == 1) {
    return(paste0("'", acronyms[1], "'"))
  } else if (n == 2) {
    return(paste0("'", acronyms[1], "' and '", acronyms[2], "'"))
  } else {
    # For more than 2 acronyms, handle commas and 'and' appropriately
    return(paste0("'", paste(acronyms[-n], collapse = "', '"), "' and '", acronyms[n], "'"))
  }
}

