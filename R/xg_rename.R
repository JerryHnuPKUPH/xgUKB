#' Rename columns from UKB Field IDs to Descriptions with smart suffix handling
#'
#' @param data A data.frame or data.table with raw column names.
#' @param dict The dictionary data frame. MUST contain columns 'FieldID' and 'Description'.
#' @return The data with renamed columns.
#' @export
xg_rename <- function(data, dict = ukb_dict_mini) {
  # 0. Check Dependencies
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' required.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' required.")
  
  # 1. Prepare Dictionary
  if (is.null(dict$FieldID)) stop("The dictionary provided does not have a 'FieldID' column.")
  
  # Clean dictionary entries
  valid_dict <- dict[!is.na(dict$FieldID) & trimws(as.character(dict$FieldID)) != "", ]
  valid_dict$FieldID <- as.character(valid_dict$FieldID)
  
  # Helper: Clean text for variable names
  clean_text <- function(x) {
    if (length(x) == 0) return(character(0))
    x[is.na(x)] <- ""
    x <- gsub("[^A-Za-z0-9]", "_", x) # Special chars to underscore
    x <- gsub("_+", "_", x)           # Reduce multiple underscores
    x <- gsub("_$", "", x)            # Trim trailing
    x <- gsub("^_", "", x)            # Trim leading
    return(x)
  }
  
  valid_dict$CleanDesc <- clean_text(valid_dict$Description)
  
  # Handle missing descriptions
  missing_desc_mask <- is.na(valid_dict$CleanDesc) | valid_dict$CleanDesc == "" | valid_dict$CleanDesc == "NA"
  if ("Name" %in% names(valid_dict)) {
    fallback_names <- valid_dict$Name[missing_desc_mask]
    fallback_names[is.na(fallback_names)] <- paste0("Field_", valid_dict$FieldID[missing_desc_mask]) 
    valid_dict$CleanDesc[missing_desc_mask] <- fallback_names
  } else {
    valid_dict$CleanDesc[missing_desc_mask] <- paste0("Field_", valid_dict$FieldID[missing_desc_mask])
  }
  
  # Build Lookups
  # Case 1: Handle duplicate descriptions in dictionary (e.g. different IDs mapping to "Pulse")
  desc_counts_dict <- table(valid_dict$CleanDesc)
  duplicate_descs_dict <- names(desc_counts_dict[desc_counts_dict > 1])
  
  id_to_desc <- setNames(valid_dict$CleanDesc, valid_dict$FieldID)
  id_to_varname <- setNames(paste0("p", valid_dict$FieldID), valid_dict$FieldID)
  
  # 2. Pre-scan Data to Determine Instance Count
  current_names <- names(data)
  
  # Create a temporary map
  # Extract numeric ID from each column
  col_ids <- stringr::str_extract(current_names, "\\d+")
  # Find what the base description WOUL be for each column
  potential_descs <- id_to_desc[col_ids]
  
  # Filter out non-matches and special columns for the count
  valid_indices <- !is.na(potential_descs) & !tolower(current_names) %in% c("eid", "subject_id", "id")
  # Count how many times each Description appears in the INPUT data
  real_data_desc_counts <- table(potential_descs[valid_indices])
  
  # 3. Generate New Names
  new_names <- sapply(current_names, function(col_name) {
    # Check Special Columns
    if (tolower(col_name) %in% c("eid", "subject_id", "id")) return("Subject_ID")
    
    col_id <- stringr::str_extract(col_name, "\\d+")
    
    # Fail-safe: No ID, keep name
    if (is.na(col_id)) return(col_name)
    
    # Fail-safe: ID not in dictionary, keep name
    base_desc <- id_to_desc[col_id]
    if (is.null(base_desc) || is.na(base_desc)) return(col_name)
    
    # --- LOGIC START ---
    
    # 1. Base Name Resolution (Handle Dictionary Duplicates)
    if (base_desc %in% duplicate_descs_dict) {
      final_base <- paste0(base_desc, "_", id_to_varname[col_id])
    } else {
      final_base <- base_desc
    }
    
    # 2. Suffix Logic (Handle Instances)
    occurrence_count <- real_data_desc_counts[base_desc]
    
    # Condition: If Description appears > 1 time in data (e.g. _i0 and _i1), KEEP suffix.
    if (!is.na(occurrence_count) && occurrence_count > 1) {
      
      # *** STANDARDIZATION LOGIC (UPDATED) ***
      # Instead of purely blindly cutting, try to extract standard Instance info
      
      # Check if it already looks like standard UKB tool format (pXXXX_i0)
      if (grepl("_i\\d+", col_name)) {
        # Ideally extract the '_i0' or '_i0_a0' part
        extracted_suffix <- stringr::str_extract(col_name, "_i\\d+.*$")
        final_name <- paste0(final_base, extracted_suffix)
        
      } else if (grepl(paste0("\\.", col_id, "\\."), col_name)) {
        # Handle raw UKB format: f.30600.0.0 -> ID=30600, Inst=0
        # Extract the number immediately following the ID
        # Regex explanation: Look for dot, ID, dot, (digits), dot/end
        inst_match <- stringr::str_match(col_name, paste0("\\.", col_id, "\\.(\\d+)"))
        if (!is.na(inst_match[1,2])) {
          inst_num <- inst_match[1,2]
          final_name <- paste0(final_base, "_i", inst_num) # Enforce _i format
        } else {
          # Fallback if structure is weird
          pattern_remove <- paste0("^[^0-9]*", col_id)
          suffix <- sub(pattern_remove, "", col_name)
          final_name <- paste0(final_base, suffix)
        }
      } else {
        # Fallback for others (e.g. just raw suffix preservation)
        pattern_remove <- paste0("^[^0-9]*", col_id)
        suffix <- sub(pattern_remove, "", col_name)
        final_name <- paste0(final_base, suffix)
      }
      
    } else {
      # Occurs only ONCE -> Clean name (Drop suffix)
      final_name <- final_base
    }
    
    # Final sanity check against NA result
    if (is.na(final_name) || grepl("^NA_", final_name)) return(col_name)
    
    return(final_name)
  })
  
  # 4. Final Safety Check & Apply
  dt <- data.table::copy(data)
  
  # Ensure uniqueness (just in case edge cases remain)
  if (any(duplicated(new_names))) {
    warning("xg_rename: Some names were still duplicates. Appending unique index.")
    new_names <- make.unique(new_names, sep = "_dup")
  }
  
  data.table::setnames(dt, current_names, new_names)
  
  return(dt)
}