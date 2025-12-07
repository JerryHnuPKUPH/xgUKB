#' Quick Load & Clean UKB Data
#'
#' Extracts specified variables from large CSV/Tab files and automatically formats the data.
#'
#' @param file_path Local path to UKB data file (.csv, .tab, .txt).
#' @param field_ids Vector of Field IDs to extract (e.g., c("31", "21001")).
#' @param eid_col Primary key column name, defaults to "f.eid".
#' @return Cleaned data.frame (tibble)
#' @export
xg_extract <- function(file_path, field_ids, eid_col = "f.eid") {
  requireNamespace("data.table")
  
  message(">>> Scanning file headers...")
  # Read only first row to get column names
  all_headers <- names(data.table::fread(file_path, nrows = 0))
  
  # Construct target column list
  # Always include eid
  target_cols <- c(eid_col)
  
  for (fid in field_ids) {
    # Match f.XXXX.x.x format
    found <- grep(paste0("^f\\.", fid, "\\."), all_headers, value = TRUE)
    if (length(found) > 0) {
      target_cols <- c(target_cols, found)
    } else {
      warning(paste("Field ID not found in file:", fid))
    }
  }
  
  target_cols <- unique(target_cols)
  
  message(paste0(">>> Extracting ", length(target_cols), " columns (using data.table for efficiency)..."))
  
  # Use fread's select parameter to read specific columns (memory-efficient)
  dt <- data.table::fread(file_path, select = target_cols)
  
  # Convert to tibble for downstream processing
  df <- dplyr::as_tibble(dt)
  
  message(">>> Extraction complete!")
  return(df)
}

#' Auto-Merge Baseline & Follow-up Data
#'
#' Merges instances (e.g., f.xxxx.0.0 [baseline], f.xxxx.1.0 [first follow-up])
#' into single columns using either first non-NA value or row means.
#' Mimics ukhelp's auto-cleaning logic.
#'
#' @param df Input data frame
#' @param field_id Target Field ID
#' @param method Merge method: "first" (prioritize baseline, use follow-up if missing) or "mean" (row average)
#' @export
xg_merge_instances <- function(df, field_id, method = "first") {
  # Find all columns for this field
  cols <- grep(paste0("^f\\.", field_id, "\\."), names(df), value = TRUE)
  
  if (length(cols) == 0) return(NULL)
  
  # First non-NA implementation (Coalesce)
  if (method == "first") {
    # Dynamically construct coalesce call
    res_vec <- dplyr::coalesce(!!!dplyr::select(df, dplyr::all_of(cols)))
    return(res_vec)
  }
  
  # Row mean implementation
  if (method == "mean") {
    res_vec <- rowMeans(dplyr::select(df, dplyr::all_of(cols)), na.rm = TRUE)
    # Convert NaN back to NA
    res_vec[is.nan(res_vec)] <- NA
    return(res_vec)
  }
}
