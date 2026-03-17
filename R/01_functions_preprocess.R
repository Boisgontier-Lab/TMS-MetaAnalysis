#' Import correlation data from CSV
#' 
#' @param filename Name of the CSV file (without path)
#' @param data_dir Path to data directory (default: "data")
#' @return Data frame with imported data
import_correlation_data <- function(filename, data_dir = "../data") {
  filepath <- file.path(data_dir, filename)
  data <- read.csv(filepath, header = TRUE, sep = ",")
  return(data)
}

#' Calculate Fisher's z transformation and variance
#' 
#' Applies Fisher's z transformation to correlation coefficients
#' and calculates the sampling variance for meta-analysis.
#' 
#' @param data Data frame containing 'cor' and 'n' columns
#' @return Data frame with added z, var.z, and cor_id columns
calculate_fisher_z <- function(data) {
  data$z <- atanh(data$cor)
  data$var.z <- 1 / (data$n - 3)
  data$cor_id <- 1:nrow(data)
  return(data)
}

#' Import and preprocess correlation data
#' 
#' Combines import and Fisher's z calculation in one step
#' 
#' @param filename Name of the CSV file
#' @param data_dir Path to data directory
#' @param save_processed Whether to save processed data back to file
#' @return Preprocessed data frame
import_and_preprocess <- function(filename, data_dir = "../data", save_processed = FALSE) {
  data <- import_correlation_data(filename, data_dir)
  data <- calculate_fisher_z(data)
  
  if (save_processed) {
    filepath <- file.path(data_dir, filename)
    write.csv(data, filepath, row.names = FALSE)
  }
  
  return(data)
}
