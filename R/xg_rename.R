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
  valid_dict <- dict
  if (is.null(valid_dict$Description)) stop("Dictionary must have a 'Description' column.")
  
  # --- HELPER: Logic to extract the "clean" name from Description
  get_clean_desc <- function(desc_vec) {
    sapply(desc_vec, function(x) {
      if (is.na(x) || x == "") return(NA)
      # Priority: Olink style (before semicolon)
      if (grepl(";", x)) {
        clean_val <- trimws(sub(";.*", "", x))
      } else {
        clean_val <- x
      }
      # Clean special chars
      clean_val <- gsub("[^A-Za-z0-9]", "_", clean_val)
      clean_val <- gsub("_+", "_", clean_val)
      clean_val <- gsub("_$", "", clean_val)
      clean_val <- gsub("^_", "", clean_val)
      return(clean_val)
    }, USE.NAMES = FALSE)
  }
  
  valid_dict$CleanDesc <- get_clean_desc(valid_dict$Description)
  
  # Fill missing descriptions
  missing_desc <- is.na(valid_dict$CleanDesc) | valid_dict$CleanDesc == "" | valid_dict$CleanDesc == "NA"
  if ("Name" %in% names(valid_dict)) {
    valid_dict$CleanDesc[missing_desc] <- valid_dict$Name[missing_desc]
  } else if ("FieldID" %in% names(valid_dict)) {
    valid_dict$CleanDesc[missing_desc] <- paste0("Field_", valid_dict$FieldID[missing_desc])
  }
  
  # Build Lookup Tables (Force lowercase keys for robust matching)
  name_to_desc <- NULL
  if ("Name" %in% names(valid_dict)) {
    tmp <- valid_dict[!is.na(valid_dict$Name) & valid_dict$Name != "", ]
    # Key: Lowercase Name, Value: Clean Description
    if (nrow(tmp) > 0) {
      name_to_desc <- setNames(tmp$CleanDesc, tolower(trimws(tmp$Name)))
    }
  }
  
  id_to_desc <- NULL
  if ("FieldID" %in% names(valid_dict)) {
    tmp <- valid_dict[!is.na(valid_dict$FieldID), ]
    if (nrow(tmp) > 0) {
      id_to_desc <- setNames(tmp$CleanDesc, as.character(tmp$FieldID))
    }
  }
  
  # --- TARGET NAME RESOLUTION ---
  current_names <- names(data)
  
  # Calculate target names for all columns
  target_bases <- sapply(current_names, function(col_name) {
    col_lower <- tolower(col_name) # Work with lowercase inputs
    
    # Skip special ID columns
    if (col_lower %in% c("eid", "subject_id", "id")) return("Subject_ID")
    
    # Strategy A: Name Match (Priority)
    if (!is.null(name_to_desc) && col_lower %in% names(name_to_desc)) {
      return(name_to_desc[[col_lower]])
    }
    
    # Strategy B: Field ID Match
    # Extract first numeric sequence
    col_id <- stringr::str_extract(col_name, "\\d+")
    if (!is.na(col_id) && !is.null(id_to_desc) && col_id %in% names(id_to_desc)) {
      return(id_to_desc[[col_id]])
    }
    
    return(NA_character_) # Explicitly return NA character
  })
  
  # --- CHECK FOR ZERO MATCHES (Fixes the "nothing to tabulate" error) ---
  valid_targets <- target_bases[!is.na(target_bases) & target_bases != "Subject_ID"]
  
  if (length(valid_targets) == 0) {
    message("Warning: xg_rename could not match any columns using the provided dictionary.")
    message("Top 5 columns in data: ", paste(head(current_names, 5), collapse=", "))
    if (!is.null(name_to_desc)) {
      message("Sample dictionary Names: ", paste(head(names(name_to_desc), 5), collapse=", "))
    }
    message("Returning original data unchanged.")
    return(data)
  }
  
  # Count occurrences to handle duplicates
  real_data_desc_counts <- table(valid_targets)
  
  # --- RENAME EXECUTION ---
  new_names <- sapply(seq_along(current_names), function(i) {
    col_name <- current_names[i]
    base_desc <- target_bases[i]
    
    # 1. No match or Special
    if (is.na(base_desc)) return(col_name)
    if (base_desc == "Subject_ID") return("Subject_ID")
    
    # 2. Check collisions
    count <- real_data_desc_counts[[base_desc]]
    
    if (!is.na(count) && count > 1) {
      # COLLISION: Need suffix
      if (grepl("_i\\d+", col_name)) {
        suffix <- stringr::str_extract(col_name, "_i\\d+.*$")
      } else {
        col_id <- stringr::str_extract(col_name, "\\d+")
        inst_match <- stringr::str_match(col_name, paste0("\\.", col_id, "\\.(\\d+)"))
        if (!is.na(inst_match[1,2])) {
          suffix <- paste0("_i", inst_match[1,2])
        } else {
          suffix <- paste0("_", col_name) # Fallback
        }
      }
      return(paste0(base_desc, suffix))
    } else {
      # UNIQUE
      return(base_desc)
    }
  })
  
  # 4. Final Safety Check
  dt <- data.table::copy(data)
  
  if (any(duplicated(new_names))) {
    warning("xg_rename: Duplicate names generated. Appending unique index.")
    new_names <- make.unique(new_names, sep = "_dup")
  }
  
  data.table::setnames(dt, current_names, new_names)
  return(dt)
}