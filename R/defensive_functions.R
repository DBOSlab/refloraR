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

