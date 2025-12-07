#' Search Variable Dictionary
#'
#' Fuzzy search the UK Biobank variable dictionary based on keywords, eliminating the need to open the web-based Showcase.
#'
#' @param keywords One or more character string keywords (e.g., c("sex", "age")). Supports Chinese or English (depending on the content of the dictionary).
#' @param view Logical. Whether to display the results in the View window. Defaults to TRUE.
#' @return Returns a data.frame containing information such as FieldID, Description, etc.
#' @export
#' @examples
#' \dontrun{
#' xg_find("diabetes")
#' xg_find(c("血压", "blood pressure"))
#' }
xg_find <- function(keywords, view = TRUE) {
  # Check if the built-in dictionary exists
  if (!exists("ukb_dict_mini")) {
    stop("Error: Built-in dictionary `ukb_dict_mini` not found. Please ensure the package is correctly loaded.")
  }
  
  # Construct regex pattern (Case-insensitive)
  pattern <- paste(keywords, collapse = "|")
  
  res <- ukb_dict_mini %>%
    dplyr::filter(stringr::str_detect(Description, stringr::regex(pattern, ignore_case = TRUE)) | 
                    stringr::str_detect(FieldID, pattern))
  
  message(paste0(">>> Found ", nrow(res), " matching variables."))
  
  if (view) {
    View(res)
  }
  return(res)
}

#' Get Complete Column Names for a Specific Field ID
#'
#' Takes a Field ID (e.g., "21001") as input and returns the actual column names from the dataset (e.g., "f.21001.0.0").
#' @param field_id Character or numeric.
#' @param data_cols A vector containing all column names of the dataset.
#' @return A vector of matched column names.
#' @export
xg_get_cols <- function(field_id, data_cols) {
  pattern <- paste0("^f\\.", field_id, "\\.")
  grep(pattern, data_cols, value = TRUE)
}