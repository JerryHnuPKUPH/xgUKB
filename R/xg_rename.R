#' Rename columns based on UKB Dictionary (Field ID or Name)
#'
#' @description
#' Rename columns in a dataset using a UKB dictionary. 
#' It supports two modes of matching:
#' 1. **Field ID Matching**: Matches columns like "p30600_i0" to FieldID "30600".
#' 2. **Name Matching** (Olink style): Matches columns like "il6" to Name "il6".
#' 
#' For Olink data, it extracts the short protein name from the Description 
#' (the part before the first semicolon).
#'
#' @author Jun Xu <xujun05@pku.edu.cn>
#'
#' @param data A data.frame or data.table with raw column names.
#' @param dict The dictionary data frame. MUST contain 'Description'. 
#'             Should contain 'FieldID' and/or 'Name'.
#' @return The data with renamed columns.
#' @export
xg_rename <- function(data, dict = ukb_dict_mini) {
  # 0. Check Dependencies
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' required.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' required.")
  
  # 1. Prepare Dictionary
  # Clean up dictionary
  valid_dict <- dict
  
  # Ensure Description exists
  if (is.null(valid_dict$Description)) stop("Dictionary must have a 'Description' column.")
  
  # --- HELPER: Logic to extract the "clean" name from Description
  # Priority: 
  # 1. If semicolon exists (Olink style "IL6;Interleukin-6"), take part before ';'
  # 2. Otherwise use full cleaned description
  get_clean_desc <- function(desc_vec) {
    sapply(desc_vec, function(x) {
      if (is.na(x) || x == "") return(NA)
      # Check for semicolon
      if (grepl(";", x)) {
        # Olink style: Extract part before first semicolon
        clean_val <- trimws(sub(";.*", "", x))
      } else {
        # Standard UKB style
        clean_val <- x
      }
      
      # Clean special chars to underscores
      clean_val <- gsub("[^A-Za-z0-9]", "_", clean_val)
      clean_val <- gsub("_+", "_", clean_val)
      clean_val <- gsub("_$", "", clean_val)
      clean_val <- gsub("^_", "", clean_val)
      return(clean_val)
    })
  }
  
  valid_dict$CleanDesc <- get_clean_desc(valid_dict$Description)
  
  # Handle NAs in Description -> Fallback to Name or FieldID
  missing_mask <- is.na(valid_dict$CleanDesc) | valid_dict$CleanDesc == ""
  
  if ("Name" %in% names(valid_dict)) {
    valid_dict$CleanDesc[missing_mask] <- valid_dict$Name[missing_mask]
  } else if ("FieldID" %in% names(valid_dict)) {
    valid_dict$CleanDesc[missing_mask] <- paste0("Field_", valid_dict$FieldID[missing_mask])
  }
  
  # Build Lookup Tables
  # Map 1: Name -> CleanDesc (For Olink data like 'il6', 'hnrnpk')
  name_to_desc <- if ("Name" %in% names(valid_dict)) {
    # Remove rows with empty names and create map
    tmp <- valid_dict[!is.na(valid_dict$Name) & valid_dict$Name != "", ]
    setNames(tmp$CleanDesc, tmp$Name)
  } else {
    NULL
  }
  
  # Map 2: FieldID -> CleanDesc (For standard UKB data like 'p30600')
  id_to_desc <- if ("FieldID" %in% names(valid_dict)) {
    tmp <- valid_dict[!is.na(valid_dict$FieldID), ]
    # Ensure FieldID is char for matching
    setNames(tmp$CleanDesc, as.character(tmp$FieldID)) 
  } else {
    NULL
  }
  
  # --- INSTANCE COUNT LOGIC ---
  # To decide whether to keep suffix or not, we need to know the *Target Name* collision count
  current_names <- names(data)
  
  # Step 1: Pre-calculate what each column *wants* to be renamed to
  target_bases <- sapply(current_names, function(col_name) {
    if (tolower(col_name) %in% c("eid", "subject_id", "id")) return("Subject_ID")
    
    # Strategy A: Exact Name Match (Priority for Olink)
    # Note: Dictionary Name might be user-provided via input, assume case-sensitive or try both?
    # Let's try exact match first, then lowercase match
    if (!is.null(name_to_desc)) {
      if (col_name %in% names(name_to_desc)) return(name_to_desc[[col_name]])
      # Try lowercase match if dictionary has names
      if (tolower(col_name) %in% names(name_to_desc)) return(name_to_desc[[tolower(col_name)]])
    }
    
    # Strategy B: Field ID Match (Standard UKB)
    col_id <- stringr::str_extract(col_name, "\\d+")
    if (!is.na(col_id) && !is.null(id_to_desc)) {
      if (col_id %in% names(id_to_desc)) return(id_to_desc[[col_id]])
    }
    
    return(NA) # No match found
  })
  
  # Step 2: Count global occurrences of each target Base Name
  # If "IL6" appears twice (from different raw columns), we must keep suffixes.
  real_data_desc_counts <- table(target_bases[!is.na(target_bases)])
  
  # --- RENAME EXECUTION ---
  new_names <- sapply(seq_along(current_names), function(i) {
    col_name <- current_names[i]
    base_desc <- target_bases[[i]]
    
    # 1. Skip if special or no match
    if (tolower(col_name) %in% c("eid", "subject_id", "id")) return("Subject_ID")
    if (is.na(base_desc)) return(col_name)
    
    # 2. Check collisions
    # Does this Target Name appear multiple times in this dataset?
    count <- real_data_desc_counts[[base_desc]]
    
    if (!is.na(count) && count > 1) {
      # COLLISION: Need suffix
      # Try to find a meaningful suffix
      
      # Is there an explicit instance _i0, _i1?
      if (grepl("_i\\d+", col_name)) {
        suffix <- stringr::str_extract(col_name, "_i\\d+.*$")
      } else {
        # Fallback: Does it look like f.xxxxx.0.0?
        col_id <- stringr::str_extract(col_name, "\\d+")
        inst_match <- stringr::str_match(col_name, paste0("\\.", col_id, "\\.(\\d+)"))
        if (!is.na(inst_match[1,2])) {
          suffix <- paste0("_i", inst_match[1,2])
        } else {
          # Last resort: just whatever is left after removing the matching part?
          # Hard to define "matching part" for 'Name' match mode.
          # Simplest safe fallback: append original name to keep unique
          suffix <- paste0("_", col_name)
        }
      }
      final_name <- paste0(base_desc, suffix)
    } else {
      # UNIQUE: Clean name
      final_name <- base_desc
    }
    
    return(final_name)
  })
  
  # 4. Final Safety Check
  dt <- data.table::copy(data)
  
  # Ensure uniqueness (Duplicate columns protection)
  if (any(duplicated(new_names))) {
    warning("xg_rename: Duplicate names generated. Appending unique index.")
    new_names <- make.unique(new_names, sep = "_dup")
  }
  
  data.table::setnames(dt, current_names, new_names)
  
  return(dt)
}