#' Extract Specific Fields from UKB Data (Support ID or Name)
#'
#' @description
#' Efficiently reads a UK Biobank dataset (CSV/Tab-separated) and extracts specific columns.
#' It supports two types of extraction identifiers:
#' 1. **Numeric Field IDs** (e.g., 30600) -> Extracts standard formats like "f.30600.0.0" or "p30600_i0".
#' 2. **Variable Names** (e.g., "a1bg", "il6") -> Extracts columns matching the name exactly or with instance suffixes.
#'
#' @author Jun Xu <xujun05@pku.edu.cn>
#'
#' @param file Path to the UKB data file (csv, txt, tab, etc.).
#' @param field_ids A vector of identifiers. Can be numeric IDs (e.g., c(30600, 30601)) or character Names (e.g., c("a1bg", "BMI")).
#' @param instance Numeric vector of instances to extract (e.g., 0, 1, or c(0,1)). 
#'                 Only applies strictly to Numeric Field IDs, or Names with standard suffix patterns.
#' @return A data.table containing the EID (subject ID) and the requested columns.
#' @export
xg_extract <- function(file, field_ids, instance = 0) {
  
  # 0. Dependencies
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' required.")
  
  message(">>> Scanning header...")
  
  # 1. Read Header Only to determine available columns
  # This is much faster than guessing patterns blindly
  header <- names(data.table::fread(file, nrows = 0))
  
  # Identify the ID column (eid)
  # Try common variations: "eid", "f.eid", "id"
  eid_col <- grep("^(eid|f\\.eid|id|subject_id)$", header, ignore.case = TRUE, value = TRUE)
  if (length(eid_col) == 0) {
    warning("Could not automatically identify an 'eid' or 'id' column. Extracts might lack subject identifiers.")
    eid_col <- character(0)
  } else {
    eid_col <- eid_col[1] # Take the first match
  }
  
  cols_to_extract <- c()
  
  # 2. Iterate through requested IDs/Names to find matches
  # Using a loop to handle each ID specifically allows for mixed types (IDs and Names)
  
  for (req_id in field_ids) {
    req_id <- as.character(req_id)
    matches <- c()
    
    # --- Logic A: Input is a Numeric string (Standard Field ID) ---
    if (grepl("^\\d+$", req_id)) {
      # Construct regex for standard UKB patterns: f.30600.0.0 or p30600_i0
      # Strict adherence to 'instance' parameter
      inst_patterns <- paste0(c(
        paste0("^f\\.", req_id, "\\.", instance, "\\."),  # f.ID.inst.array
        paste0("^p", req_id, "_i", instance)              # pID_inst...
      ), collapse = "|")
      
      matches <- grep(inst_patterns, header, value = TRUE)
      
    } else {
      # --- Logic B: Input is a Character Name (e.g., "a1bg", "BMI") ---
      # 1. Exact match (Case-insensitive) e.g., "a1bg" matches "a1bg" or "A1BG"
      exact_match <- header[tolower(header) == tolower(req_id)]
      
      # 2. Name with standard suffixes (if users ask for "a1bg" but file has "a1bg_i0")
      # We check if the name + instance pattern exists
      suffix_pattern <- paste0("^", req_id, "(_i|\\.)[", paste(instance, collapse="|"), "]")
      suffix_matches <- grep(suffix_pattern, header, ignore.case = TRUE, value = TRUE)
      
      matches <- unique(c(exact_match, suffix_matches))
    }
    
    # Store matches
    if (length(matches) > 0) {
      cols_to_extract <- c(cols_to_extract, matches)
    } else {
      warning(paste("Field ID/Name:", req_id, "not found in file header."))
    }
  }
  
  # Combine EID with found columns
  final_cols <- unique(c(eid_col, cols_to_extract))
  
  if (length(final_cols) == length(eid_col)) {
    stop("No data columns found to extract. Check your IDs/Names or Instance numbers.")
  }
  
  message(paste(">>> Identifying", length(final_cols), "columns (including ID)..."))
  
  # 3. Read Data
  dt <- data.table::fread(file, select = final_cols)
  
  return(dt)
}