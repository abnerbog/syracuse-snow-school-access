#' @title Extract Specific Files from a ZIP Archive
#' @description This function unzips an archive, filters for files matching a regular expression, and copies those files from a temporary directory to a specified output directory.
#'
#' @param zip_file A character string representing the file path to the input ZIP archive.
#' @param out_dir A character string representing the path to the desired output directory for the extracted files.
#' @param file_name_regex A character string containing a regular expression to match the names of the files to be extracted (e.g., ".pts" to get shapefile points).
#'
#' @return A character vector of the file paths for the newly copied, extracted files in the `out_dir`.
#'
extract_files_from_zip <- function(zip_file, out_dir, file_name_regex) {
  
  zipdir <- tempdir()
  
  # List all files within the zip archive
  all_files_in_zip <- unzip(zipfile = zip_file, list = TRUE)$Name
  
  # Filter files based on the provided regular expression
  # This is where the generalization happens
  files_to_extract <- grep(file_name_regex, all_files_in_zip, ignore.case = TRUE, value = TRUE)
  
  # Unzip and extract the identified files
  zip::unzip(zipfile = zip_file,
             files = files_to_extract,
             exdir = zipdir)
  
  # Copy extracted files from the temporary directory to the desired output directory
  extracted_paths <- file.path(zipdir, files_to_extract)
  copied_paths <- file.path(out_dir, basename(files_to_extract))
  
  file.copy(from = extracted_paths,
            to = copied_paths,
            overwrite = TRUE) # Set to TRUE if you want to overwrite existing files
  
  return(copied_paths)
}

#' @title Count Survey Responses
#' @description Summarizes a dataframe column containing survey responses, calculating frequencies and percentages. 
#'   It dynamically handles both single-choice questions and multiple-choice questions (where responses are combined in a single string separated by a delimiter).
#'
#' @param data A dataframe or tibble containing the survey responses.
#' @param col The unquoted name of the column to be processed (e.g., `question_1`).
#' @param delim A character string used to split multiple responses in a single cell. Defaults to `NULL` (no splitting). Commonly `","`, `";"`, or `regex("\\s*,\\s*")`.
#'
#' @return A tibble with three columns: the unique responses from the target column, `n` (frequency count), and `percent` (relative frequency).
#' @import dplyr tidyr stringr
#' @export
#' 
#' @examples
#' # For single-response columns (no delimiter needed)
#' # count_survey_responses(df, single_choice_col)
#' 
#' # For multi-response columns (split by comma)
#' # count_survey_responses(df, multi_choice_col, delim = ",")
count_survey_responses <- function(data, col, delim = NULL) {
  
  # Start the pipeline
  df <- data
  
  # 1. Conditionally split the column if a delimiter is provided
  if (!is.null(delim)) {
    df <- df %>%
      tidyr::separate_longer_delim(cols = {{ col }}, delim = delim)
  }
  
  # 2. Clean, count, and calculate
  df %>%
    # Use across() with {{ col }} to apply str_trim dynamically to the chosen column
    dplyr::mutate(dplyr::across({{ col }}, stringr::str_trim)) %>%
    
    # Optional: Filter out empty strings that might result from trailing delimiters
    dplyr::filter({{ col }} != "") %>% 
    
    # Generate counts
    dplyr::count({{ col }}, .drop = FALSE, sort = TRUE) %>%
    
    # Calculate percentages
    dplyr::mutate(percent = 100 * n / sum(n))
}