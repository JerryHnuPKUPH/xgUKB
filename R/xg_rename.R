#' Rename columns based on UKB Dictionary (Field ID or Name)
#'
#' @description
#' Rename columns in a dataset using a UKB dictionary. 
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
  
  # --- LOGIC: Extract "clean" name from Description
  get_clean_desc <- function(desc_vec) {
    sapply(desc_vec, function(x) {
      if (is.na(x) || x == "") return(NA)
      # 1. Olink style: Extract part before first semicolon
      if (grepl(";", x)) {
        val <- trimws(sub(";.*", "", x))
      } else {
        val <- x
      }
      # 2. Clean special chars (replace spaces/symbols with _)
      val <- gsub("[^A-Za-z0-9]", "_", val)
      # 3. Handle strict Olink/Gene name convention (usually ALL CAPS for Description)
      # But user wants whatever is before ";". Usually "IL6;..." -> "IL6"
      return(val)
    }, USE.NAMES = FALSE)
  }
  
  valid_dict$CleanDesc <- get_clean_desc(valid_dict$Description)
  
  # --- BUILD LOOKUP TABLE (The "Map") ---
  # Key: Normalized Name (Lower case, trimmed) -> Value: Clean Description
  
  lookup_map <- list()
  
  # 1. Add 'Name' based mapping (Priority for Olink)
  if ("Name" %in% names(valid_dict)) {
    # Extract valid names
    tmp_name <- valid_dict[!is.na(valid_dict$Name) & valid_dict$Name != "", ]
    if (nrow(tmp_name) > 0) {
      # Normalize keys: lowercase + trimmed
      keys <- tolower(trimws(tmp_name$Name))
      vals <- tmp_name$CleanDesc
      # Assign to map (handling duplicates by keeping first found)
      lookup_map[keys] <- vals 
    }
  }
  
  # 2. Add 'FieldID' based mapping (Secondary)
  if ("FieldID" %in% names(valid_dict)) {
    tmp_id <- valid_dict[!is.na(valid_dict$FieldID), ]
    if (nrow(tmp_id) > 0) {
      keys <- as.character(tmp_id$FieldID) # ID keys stay numeric string
      vals <- tmp_id$CleanDesc
      lookup_map[keys] <- vals
    }
  }
  
  # --- RENAME PROCESS ---
  current_names <- names(data)
  
  # Define matching function for a single column
  get_new_name <- function(col_name) {
    # 1. Handle special ID columns
    col_lower <- tolower(trimws(col_name))
    if (col_lower %in% c("eid", "subject_id", "id")) return("Subject_ID")
    
    # 2. Strategy A: Direct Name Match (e.g. "a1bg" -> "A1BG")
    if (col_lower %in% names(lookup_map)) {
      return(lookup_map[[col_lower]])
    }
    
    # 3. Strategy B: Remove common suffixes and try again
    # Try removing "_i0", ".0.0", etc.
    # Ex: "a1bg_i0" -> "a1bg" -> Lookup
    clean_base <- col_lower
    # Remove _i0, _i1...
    clean_base <- sub("_i\\d+.*$", "", clean_base)
    # Remove .0.0 pattern
    clean_base <- sub("\\.\\d+\\.\\d+$", "", clean_base)
    
    if (clean_base != col_lower && clean_base %in% names(lookup_map)) {
      return(lookup_map[[clean_base]])
    }
    
    # 4. Strategy C: Field ID Extraction
    col_id <- stringr::str_extract(col_name, "\\d+")
    if (!is.na(col_id) && col_id %in% names(lookup_map)) {
      return(lookup_map[[col_id]])
    }
    
    return(NA_character_)
  }
  
  # Calculate all targets
  target_bases <- sapply(current_names, get_new_name)
  
  # --- CHECK IF NOTHING MATCHED ---
  valid_targets <- target_bases[!is.na(target_bases) & target_bases != "Subject_ID"]
  
  if (length(valid_targets) == 0) {
    message("Warning: No columns matched the dictionary.")
    # DEBUG INFO:
    message("DEBUG: Data cols (first 3 pure): ", paste(shQuote(head(names(data),3)), collapse=", "))
    if (length(lookup_map) > 0) {
      message("DEBUG: Dictionary Keys (first 3): ", paste(shQuote(head(names(lookup_map),3)), collapse=", "))
    }
    return(data)
  }
  
  # --- HANDLE DUPLICATES (INSTANCE SUFFIXES) ---
  target_counts <- table(target_bases[!is.na(target_bases)])
  
  final_names <- sapply(seq_along(current_names), function(i) {
    old_name <- current_names[i]
    new_base <- target_bases[i]
    
    if (is.na(new_base)) return(old_name) # Keep original if no match
    if (new_base == "Subject_ID") return("Subject_ID")
    
    # Check if we need to append suffix (Collision detection)
    if (target_counts[[new_base]] > 1) {
      # Try to preserve instance info from old name
      if (grepl("_i\\d+", old_name)) {
        suffix <- stringr::str_extract(old_name, "_i\\d+")
      } else if (grepl("\\.\\d+\\.\\d+", old_name)) {
        inst <- stringr::str_extract(old_name, "(?<=\\.)\\d+(?=\\.)")
        suffix <- paste0("_i", inst)
      } else {
        suffix <- paste0("_", old_name) # Fallback
      }
      return(paste0(new_base, suffix))
    } else {
      return(new_base)
    }
  })
  
  # Apply changes
  dt <- data.table::copy(data)
  data.table::setnames(dt, current_names, final_names)
  
  return(dt)
}