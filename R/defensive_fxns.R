# Small functions to evaluate user input for the main functions and
# return meaningful errors.
# Author: Domingos Cardoso & Carlos Calderon

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
      stop(paste0("There is no Reflora-downloaded dwca folder within the directory '", path, "'."),
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

  # Accented state names are built at *runtime* with intToUtf8(), not typed
  # as \uXXXX escapes: under a non-UTF-8 session locale (e.g. LC_CTYPE=C,
  # common in CI), package sourcing has been observed to parse \uXXXX
  # escapes into the literal 8-character text "<U+00E3>" instead of the
  # intended single accented character, which no amount of post-hoc
  # Encoding<- can recover. intToUtf8() sidesteps that parse-time escape
  # resolution entirely.
  a_acute <- intToUtf8(0x00E1)  # \u00e1 -> a acute (a)
  e_acute <- intToUtf8(0x00E9)  # \u00e9 -> e acute (e)
  i_acute <- intToUtf8(0x00ED)  # \u00ed -> i acute (i)
  a_tilde <- intToUtf8(0x00E3)  # \u00e3 -> a tilde (a)
  o_circumflex <- intToUtf8(0x00F4)  # \u00f4 -> o circumflex (o)

  valid_states <- stats::setNames(
    c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS",
      "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
      "SP", "SE", "TO"),
    c("Acre", "Alagoas", paste0("Amap", a_acute), "Amazonas", "Bahia",
      paste0("Cear", a_acute), "Distrito Federal",
      paste0("Esp", i_acute, "rito Santo"), paste0("Goi", a_acute, "s"),
      paste0("Maranh", a_tilde, "o"), "Mato Grosso", "Mato Grosso do Sul",
      "Minas Gerais", paste0("Par", a_acute), paste0("Para", i_acute, "ba"),
      paste0("Paran", a_acute), "Pernambuco", paste0("Piau", i_acute),
      "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul",
      paste0("Rond", o_circumflex, "nia"), "Roraima", "Santa Catarina",
      paste0("S", a_tilde, "o Paulo"), "Sergipe", "Tocantins")
  )

  if (identical(Encoding(x), "unknown")) {
    Encoding(x) <- "UTF-8"
  }

  valid_states_full <- names(valid_states)
  valid_states_acronyms <- unname(valid_states)

  states_no_diacritics <- stringi::stri_trans_general(x, "Latin-ASCII")
  valid_states_full_no_diacritics <- stringi::stri_trans_general(valid_states_full, "Latin-ASCII")
  valid_states_acronyms_no_diacritics <- stringi::stri_trans_general(valid_states_acronyms, "Latin-ASCII")

  corrected_states <- character(length(x))

  for (i in seq_along(x)) {
    match_full <- match(states_no_diacritics[i], valid_states_full_no_diacritics)
    match_acronym <- match(states_no_diacritics[i], valid_states_acronyms_no_diacritics)

    if (!is.na(match_full)) {
      corrected_states[i] <- valid_states_full[match_full]
    } else if (!is.na(match_acronym)) {
      corrected_states[i] <- names(valid_states)[match_acronym]
    } else {
      corrected_states[i] <- x[i]  # return as-is
    }
  }

  return(corrected_states)
}


#_______________________________________________________________________________
# Check the herbarium input
.arg_check_herbarium <- function(x, verbose = verbose) {
  if (is.null(x) || length(x) == 0) return(invisible(TRUE))

  if (verbose) {
    message("Checking whether the input herbarium code exists in the Reflora...")
  }

  # Only the dcat catalog (a single request) is needed to validate herbarium
  # acronyms, so avoid the full reflora_summary(), which additionally scrapes
  # every herbarium's individual resource page for version/record counts.
  ipt_info <- .get_ipt_info(NULL)
  valid_codes <- ipt_info[[3]]

  # Check if input acronyms are valid
  invalid <- x[!x %in% valid_codes]

  if (length(invalid) > 0) {
    stop(
      sprintf(
        "The following herbarium acronym(s) are not recognized by Reflora: %s
        \nUse `reflora_summary()` to view available collections.",
        paste0(shQuote(invalid), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#_______________________________________________________________________________
# Check the level input

.arg_check_level <- function(level) {
  allowed_levels <- c("FAMILY", "GENUS")
  level_clean <- toupper(trimws(level))

  invalid <- setdiff(level_clean, allowed_levels)

  if (length(invalid) > 0) {
    stop(
      sprintf(
        "The following value(s) in `level` are invalid: %s\nAccepted values are: %s",
        paste0(shQuote(invalid), collapse = ", "),
        paste0(shQuote(allowed_levels), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(level_clean)
}

