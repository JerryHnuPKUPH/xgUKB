#' Define Disease by ICD-10
#'
#' Takes ICD prefixes as input and automatically identifies diagnosed patients from the main and secondary diagnosis columns (f.41270).
#'
#' @param df Loaded data frame (must contain f.41270 series columns).
#' @param icd_codes Character vector, e.g., c("I20", "I25").
#' @param strict Logical. TRUE = exact match, FALSE = prefix match (default).
#' @return Returns a vector consisting of 0s and 1s, where 1 indicates the presence of the disease.
#' @export
xg_define_disease <- function(df, icd_codes, strict = FALSE) {
  # Find all ICD-10 diagnosis columns (f.41270.x.x)
  icd_cols <- grep("^f\\.41270\\.", names(df), value = TRUE)
  
  if (length(icd_cols) == 0) {
    stop("Error: Relevant columns for f.41270 (ICD-10 Diagnoses) were not found in the data frame.")
  }
  
  message(">>> Scanning ICD-10 records...")
  
  # Convert data to long format implies searching across columns (this is more robust than grepping each column individually)
  # Note: This step might be slow with large datasets; using simplified apply logic here
  
  # Create a regex pattern
  if (strict) {
    pattern <- paste0("^(", paste(icd_codes, collapse = "|"), ")$")
  } else {
    # Prefix match, e.g., I25 will match I25.1, I25.9
    pattern <- paste0("^(", paste(icd_codes, collapse = "|"))
  }
  
  # Define an internal function: check if a row contains the pattern
  check_row <- function(row_data) {
    any(stringr::str_detect(na.omit(as.character(row_data)), pattern))
  }
  
  # Iterate over rows for ICD columns
  # Here df[icd_cols] needs to be a data.frame or matrix
  has_disease <- apply(df[, icd_cols], 1, check_row)
  
  return(as.integer(has_disease))
}