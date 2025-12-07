#' Extract specific fields from UKB data with instance control
#'
#' @param data A file path (string) OR a data frame.
#' @param field_ids A vector of field IDs (e.g., c("3", "20110")).
#' @param instance A vector of instance indexes to extract (e.g., 0, 1, or c(0,2)). 
#'        Default is NULL (extracts ALL instances).
#' @return A data.table containing columns that match the field IDs and instances.
#' @export
xg_extract <- function(data, field_ids, instance = NULL) {
  # 依赖检查
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required. Please install it.")
  }
  
  # --- 步骤 1: 获取表头 ---
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
  
  # --- 步骤 2: 构建匹配逻辑 ---
  target_cols <- c()
  
  # 1. 总是自动抓取 'eid'
  eid_match <- grep("^eid$|^EID$", header, value = TRUE)
  if (length(eid_match) > 0) target_cols <- c(target_cols, eid_match)
  
  # 2. 遍历 Field IDs
  for (id in field_ids) {
    if (tolower(id) == "eid") next
    
    # 清洗 ID，只保留数字
    clean_id <- gsub("[^0-9]", "", id)
    if (nchar(clean_id) == 0) next
    
    # 基础正则：匹配该 ID 的所有列
    # 兼容三种格式: p3_XX, f.3.XX, 3-XX
    base_pattern <- paste0("^p", clean_id, "_|^f\\.", clean_id, "\\.|^", clean_id, "-")
    
    # 初步筛选出所有属于该 ID 的列（包含所有 instance）
    candidates <- grep(base_pattern, header, value = TRUE)
    
    if (length(candidates) == 0) {
      warning(paste("Field ID:", id, "not found."))
      next
    }
    
    # --- 步骤 3: Instance 过滤 ---
    if (is.null(instance)) {
      # 如果没指定 instance，全都要
      target_cols <- c(target_cols, candidates)
    } else {
      # 如果指定了 instance (例如 0)，只筛选对应的列
      # 构建 instance 的正则片段
      # 针对 p3_i0 格式: "_i0"
      # 针对 f.3.0.0 格式: ".0." (注意 f.{id}.{instance}.{array})
      # 针对 3-0.0 格式: "-0."
      
      matches_instance <- c()
      
      for (inst in instance) {
        # 兼容三种不同格式的 instance 标记
        inst_patterns <- paste0(
          "_", "i", inst, "($|_|a)", "|",  # match p3_i0, p3_i0_a1
          "\\.", inst, "\\.",        "|",  # match f.3.0.0
          "-", inst, "\\."                 # match 3-0.0
        )
        
        # 在 candidates 中进一步筛选匹配该 instance 的列
        found <- grep(inst_patterns, candidates, value = TRUE)
        matches_instance <- c(matches_instance, found)
      }
      
      target_cols <- c(target_cols, matches_instance)
    }
  }
  
  target_cols <- unique(target_cols)
  
  if (length(target_cols) <= length(eid_match)) {
    warning("No data columns found to extract (check your IDs or Instance numbers).")
    # 如果只找到 eid 或者啥都没找到，可能不想报错而是返回空结果或者只有eid
    if(length(target_cols) == 0 && length(eid_match) == 0) return(NULL)
  }
  
  message(paste(">>> Identifying", length(target_cols), "columns..."))
  
  # --- 步骤 4: 提取 ---
  if (is_file_path) {
    return(data.table::fread(data, select = target_cols))
  } else {
    dt <- data.table::as.data.table(data)
    return(dt[, ..target_cols])
  }
}