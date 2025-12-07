#' Extract specific fields from UKB data with instance control
#'
#' @param data A file path (string) OR a data frame.
#' @param field_ids A vector of field IDs (e.g., c("3", "20110")).
#' @param instance A vector of instance indexes to extract (e.g., 0, 1, or c(0,2)). 
#'        Default is NULL (extracts ALL instances).
#' @return A data.table containing columns that match the field IDs and instances.
#' @export
xg_extract <- function(data, field_ids, instance = NULL) {
  # Dependency check
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required. Please install it.")
  }
  
  # --- Step 1: Get Header ---
  if (is.character(data)) {
    if (!file.exists(data)) stop("File not found: ", data)
    header <- names(data.table::fread(data, nrows = 0))
    is_file_path <- TRUE
  } else if (is.data.frame(data) || data.table::is.data.table(data)) {
    header <- names(data)
    is_file_path <- FALSE
  } else {
    stop("Input 'data' must be a file path or data frame.")
  }
  
  message(">>> Scanning header...")
  
  # --- Step 2: Build Matching Logic ---
  target_cols <- c()
  
  # 1. Always automatically extract 'eid'
  eid_match <- grep("^eid$|^EID$", header, value = TRUE)
  if (length(eid_match) > 0) target_cols <- c(target_cols, eid_match)
  
  # 2. Iterate through Field IDs
  for (id in field_ids) {
    if (tolower(id) == "eid") next
    
    # Clean ID, keep numbers only
    clean_id <- gsub("[^0-9]", "", id)
    if (nchar(clean_id) == 0) next
    
    # Base regex: match all columns for this ID
    # Compatible with three formats: p3_XX, f.3.XX, 3-XX
    base_pattern <- paste0("^p", clean_id, "_|^f\\.", clean_id, "\\.|^", clean_id, "-")
    
    # Preliminarily filter all columns belonging to this ID (including all instances)
    candidates <- grep(base_pattern, header, value = TRUE)
    
    if (length(candidates) == 0) {
      warning(paste("Field ID:", id, "not found."))
      next
    }
    
    # --- Step 3: Instance Filtering ---
    if (is.null(instance)) {
      # If instance is not specified, keep all candidates
      target_cols <- c(target_cols, candidates)
    } else {
      # If instance is specified (e.g., 0), filter only corresponding columns
      # Construct regex fragments for instances
      # For format p3_i0: "_i0"
      # For format f.3.0.0: ".0." (Note: f.{id}.{instance}.{array})
      # For format 3-0.0: "-0."
      
      matches_instance <- c()
      
      for (inst in instance) {
        # Compatible with three different instance marker formats
        inst_patterns <- paste0(
          "_", "i", inst, "($|_|a)", "|",  # match p3_i0, p3_i0_a1
          "\\.", inst, "\\.",        "|",  # match f.3.0.0
          "-", inst, "\\."                 # match 3-0.0
        )
        
        # Further filter columns in candidates that match this instance
        found <- grep(inst_patterns, candidates, value = TRUE)
        matches_instance <- c(matches_instance, found)
      }
      
      target_cols <- c(target_cols, matches_instance)
    }
  }
  
  target_cols <- unique(target_cols)
  
  if (length(target_cols) <= length(eid_match)) {
    warning("No data columns found to extract (check your IDs or Instance numbers).")
    # If only eid is found or nothing is found, allow valid return (NULL) rather than error
    if(length(target_cols) == 0 && length(eid_match) == 0) return(NULL)
  }
  
  message(paste(">>> Identifying", length(target_cols), "columns..."))
  
  # --- Step 4: Extraction ---
  if (is_file_path) {
    return(data.table::fread(data, select = target_cols))
  } else {
    dt <- data.table::as.data.table(data)
    return(dt[, ..target_cols])
  }
}