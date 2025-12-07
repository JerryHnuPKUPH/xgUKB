#' Extract specific fields from UKB data
#'
#' @param data A file path (string) OR a data frame.
#' @param field_ids A vector of field IDs (e.g., c("3", "21001") or c("eid", "p3_")).
#' @return A data.table containing columns that match the field IDs.
#' @export
xg_extract <- function(data, field_ids) {
  # 依赖检查
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required. Please install it.")
  }
  
  # --- 步骤 1: 获取表头 ---
  if (is.character(data)) {
    if (!file.exists(data)) stop("File not found: ", data)
    # 只读第0行拿列名
    header <- names(data.table::fread(data, nrows = 0))
    is_file_path <- TRUE
  } else if (is.data.frame(data) || data.table::is.data.table(data)) {
    header <- names(data)
    is_file_path <- FALSE
  } else {
    stop("Input 'data' must be a file path or data frame.")
  }
  
  message(">>> Scanning header and matching columns...")
  
  # --- 步骤 2: 智能匹配列名 ---
  target_cols <- c()
  
  # 1. 总是先尝试寻找 eid (不论用户是否在 field_ids 里写了)
  # 使用全字匹配，防止匹配到 irrelevant_eid_stuff
  eid_match <- grep("^eid$|^EID$", header, value = TRUE)
  if (length(eid_match) > 0) {
    target_cols <- c(target_cols, eid_match)
  }
  
  # 2. 遍历用户提供的 ID 进行模糊匹配
  for (id in field_ids) {
    # 如果用户传了 "eid"，我们在上面已经处理了，这里直接跳过
    if (tolower(id) == "eid") next
    
    # 核心修复：清洗 ID，只保留数字
    # 这样用户传 "p3_" 或 "3" 效果一样，都会变成 "3"
    clean_id <- gsub("[^0-9]", "", id)
    
    if (nchar(clean_id) == 0) {
      warning(paste("Skipping invalid ID:", id))
      next
    }
    
    # 构建正则：
    # ^p{id}_  -> 匹配 p3_i0 (你的格式)
    # ^f\.{id}\. -> 匹配 f.3.0.0 (标准格式)
    # ^{id}-     -> 匹配 3-0.0 (原生格式)
    pattern <- paste0("^p", clean_id, "_|^f\\.", clean_id, "\\.|^", clean_id, "-")
    
    matches <- grep(pattern, header, value = TRUE)
    
    if (length(matches) == 0) {
      warning(paste("Field ID:", id, "(searched as", clean_id, ") not found in data header."))
    } else {
      target_cols <- c(target_cols, matches)
    }
  }
  
  target_cols <- unique(target_cols) # 去重
  
  if (length(target_cols) == 0) {
    stop("No valid columns found to extract check your field_ids match the file header.")
  }
  
  message(paste(">>> Found", length(target_cols), "columns. Extracting..."))
  
  # --- 步骤 3: 提取 ---
  if (is_file_path) {
    # 只有这一步会真正读取硬盘，且只读取选中的列
    return(data.table::fread(data, select = target_cols))
  } else {
    dt <- data.table::as.data.table(data)
    return(dt[, ..target_cols])
  }
}