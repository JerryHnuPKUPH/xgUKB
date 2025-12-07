#' Search for keywords in the dictionary
#'
#' @param keyword A character vector or string to search (e.g., "BMI" or c("BMI", "Age")).
#' @param dict The dictionary data to search in (default: ukb_dict_mini).
#' @return A data frame containing the matching rows.
#' @export
xg_find <- function(keyword, dict = ukb_dict_mini) {
  
  # 1. Check if the keyword argument is valid
  if (missing(keyword) || is.null(keyword) || length(keyword) == 0) {
    stop("Please provide a keyword to search.")
  }
  
  # Filter out empty strings to prevent matching everything
  keyword <- keyword[keyword != ""]
  if (length(keyword) == 0) {
    stop("Keywords contain only empty strings.")
  }
  
  # 2. Check dictionary format
  required_cols <- c("Name", "Description", "FieldID")
  if (!all(required_cols %in% names(dict))) {
    stop("The dictionary data does not have standard columns (Name, Description, FieldID).")
  }
  
  # 3. Build multi-keyword search logic
  # Convert c("BMI", "center") to regex "BMI|center"
  # "|" represents "OR", meaning match if any one of the words is found
  search_pattern <- paste(keyword, collapse = "|")
  
  message(paste0(">>> Searching for patterns: '", search_pattern, "'"))
  
  # 4. Execute search (case-insensitive)
  # Search in Description OR in FieldID
  matches_desc <- grep(search_pattern, dict$Description, ignore.case = TRUE)
  matches_id   <- grep(search_pattern, as.character(dict$FieldID), ignore.case = TRUE)
  
  # Get union (unique indices)
  match_indices <- unique(c(matches_desc, matches_id))
  
  # 5. Return results
  result <- dict[match_indices, ]
  
  if (nrow(result) == 0) {
    message("No results found.")
    return(NULL)
  }
  
  return(result)
}