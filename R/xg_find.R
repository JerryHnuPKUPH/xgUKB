#' Search for keywords in the dictionary (Enhanced Version)
#'
#' @param keyword A character vector or string to search (e.g., "BMI" or c("BMI", "Age")).
#'                Matches ANY column (Description, ID, Levels, etc.).
#' @param dict The dictionary data to search in (default: ukb_dict_mini).
#' @param exclude A character vector or string of keywords to **exclude**. 
#'                Rows containing these words will be removed from the result.
#'                Example: exclude = "Instance 1" or exclude = c("Time", "Date").
#' @return A data frame containing the matching rows.
#' @export
xg_find <- function(keyword, dict = ukb_dict_mini, exclude = NULL) {
  
  # --- 1. Validation ---
  if (missing(keyword) || is.null(keyword) || length(keyword) == 0) {
    stop("Please provide a keyword to search.")
  }
  
  keyword <- keyword[keyword != ""]
  if (length(keyword) == 0) stop("Keywords contain only empty strings.")
  
  # --- 2. Prepare Data for Searching ---
  # Convert all columns to character to allow grep across everything (IDs, Levels, Descriptions)
  # using 'StringsAsFactors=FALSE' to be safe
  df_char <- as.data.frame(lapply(dict, as.character), stringsAsFactors = FALSE)
  
  # --- 3. Build Search Logic (Inclusion) ---
  search_pattern <- paste(keyword, collapse = "|")
  message(paste0(">>> Searching for inclusion: '", search_pattern, "'"))
  
  # Helper function: Check if pattern exists in ANY column for each row
  # This returns a logical vector (TRUE/FALSE) with length = nrow(dict)
  row_matches <- function(dataframe, pattern) {
    # Scan every column, return list of logical vectors
    matches_list <- lapply(dataframe, function(col) {
      grepl(pattern, col, ignore.case = TRUE)
    })
    # Reduce list: If TRUE in Column A OR Column B OR Column C... then Keep Row
    Reduce(`|`, matches_list)
  }
  
  # Identify rows that match the keyword
  keep_indices <- row_matches(df_char, search_pattern)
  
  # --- 4. Build Exclusion Logic (Optional) ---
  if (!is.null(exclude) && any(exclude != "")) {
    exclude <- exclude[exclude != ""]
    exclude_pattern <- paste(exclude, collapse = "|")
    message(paste0(">>> Filtering out exclusions: '", exclude_pattern, "'"))
    
    # Identify rows that match the exclude pattern
    drop_indices <- row_matches(df_char, exclude_pattern)
    
    # Logic: Keep rows that match KEYWORD AND do NOT match EXCLUDE
    final_mask <- keep_indices & (!drop_indices)
    
  } else {
    # If no exclusion needed
    final_mask <- keep_indices
  }
  
  # --- 5. Return Results ---
  result <- dict[final_mask, ]
  
  if (nrow(result) == 0) {
    message("No results found.")
    return(NULL)
  }
  
  message(paste(">>> Found", nrow(result), "matches."))
  return(result)
}