#' Rename columns from UKB Field IDs to Descriptions with smart conflict resolution
#'
#' @param data A data.frame or data.table with raw column names (e.g., p21001_i0).
#' @param dict The dictionary data frame (e.g., ukb_dict_mini).
#' @return The data with renamed columns.
#' @export
xg_rename <- function(data, dict = ukb_dict_mini) {
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' required.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' required.")
  
  # 1. Prepare dictionary
  valid_dict <- dict[!is.na(dict$FieldID), ]
  valid_dict$FieldID <- as.character(valid_dict$FieldID)
  
  # 2. Clean Descriptions
  clean_text <- function(x) {
    x <- gsub("[^A-Za-z0-9]", "_", x) # Replace special characters with underscores
    x <- gsub("_+", "_", x)           # Merge consecutive underscores
    x <- gsub("_$", "", x)            # Remove trailing underscores
    x <- gsub("^_", "", x)            # Remove leading underscores
    return(x)
  }
  valid_dict$CleanDesc <- clean_text(valid_dict$Description)
  
  # 3. Conflict Detection Logic
  # Count occurrences of descriptions
  desc_counts <- table(valid_dict$CleanDesc)
  duplicate_descs <- names(desc_counts[desc_counts > 1])
  
  # create ID -> CleanDesc mapping
  id_to_desc <- setNames(valid_dict$CleanDesc, valid_dict$FieldID)
  
  # Create ID -> Name (Original Variable Name, e.g., p21001) mapping, ensuring Name implies no instance suffix
  # Assuming the Name column is p21001, but to be safe, we force a standardized prefix for the ID here.
  # For safety, here we only take the standardized prefix corresponding to "FieldID", e.g., "p21001"
  id_to_varname <- setNames(paste0("p", valid_dict$FieldID), valid_dict$FieldID)
  
  # 4. Iterate and Rename
  current_names <- names(data)
  
  new_names <- sapply(current_names, function(col_name) {
    # Ignore Subject_ID / eid
    if (tolower(col_name) %in% c("eid", "subject_id")) return("Subject_ID")
    
    # Extract ID (match the 28538 in strings like p28538_i0)
    col_id <- stringr::str_extract(col_name, "\\d+")
    
    # Handle invalid IDs
    if (is.na(col_id) || is.null(id_to_desc[col_id])) return(col_name)
    
    # Get base description
    base_desc <- id_to_desc[col_id]
    
    # Extract suffix (e.g., _i0, _a1)
    # Logic: Remove "p"+ID (or "f."+ID) from col_name, and the remainder is the suffix
    # e.g., p28538_i0 -> remove p28538 -> remains _i0
    # e.g., f.28538.0.0 -> remove f.28538 -> remains .0.0
    # Regex: Match the part "non-digit character + ID" and replace it with empty string
    pattern_remove <- paste0("^[^0-9]*", col_id)
    suffix <- sub(pattern_remove, "", col_name)
    
    # --- Core Logic Fix ---
    
    # Check for duplicate name conflicts
    is_duplicate <- base_desc %in% duplicate_descs
    
    if (is_duplicate) {
      # If conflict exists: Use "Description_OriginalID" + "Suffix"
      # new_base = Desc_p28538
      new_base <- paste0(base_desc, "_", id_to_varname[col_id])
    } else {
      # If no conflict: Use "Description"
      new_base <- base_desc
    }
    
    # Append suffix
    final_name <- paste0(new_base, suffix)
    
    # Prevent double suffixes (This is key to solving _i0_i0 issues)
    # Check if new_base already contains the suffix (unlikely but for robustness)
    # Or if the suffix is effectively empty.
    # Most importantly: ensure id_to_varname does not carry _i0
    
    return(final_name)
  })
  
  # 5. Apply and De-duplicate
  dt <- data.table::copy(data)
  data.table::setnames(dt, names(dt), new_names)
  
  # Final fallback if names are still duplicated by chance
  if (any(duplicated(names(dt)))) {
    warning("Duplicate names found after rename. Appending numeric index.")
    data.table::setnames(dt, names(dt), make.unique(names(dt), sep = "_"))
  }
  
  return(dt)
}