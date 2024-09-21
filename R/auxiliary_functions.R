# Auxiliary functions to support main functions
# Author: Domingos Cardoso

#_______________________________________________________________________________
### Function save csv files ###

saveCSV <- function(df,
                    verbose = TRUE,
                    filename = NULL,
                    dir = dir) {

  # Save the data frame if param save is TRUE
  # Create a new directory to save the results with current date
  # If there is no directory... make one!

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  # Create and save the spreadsheet in .csv format
  if (verbose) {
    message(paste0("Writing spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  write.csv(df, file = paste0(dir, "/", filename), row.names = FALSE)

}

