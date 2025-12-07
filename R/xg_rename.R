#' Rename columns from UKB Field IDs to Descriptions
#'
#' @param data A data.frame or data.table with raw column names (e.g., p21001_i0).
#' @param dict The dictionary data frame (e.g., ukb_dict_mini).
#' @return The data with renamed columns.
#' @export
xg_rename <- function(data, dict = ukb_dict_mini) {
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("data.table", quietly = TRUE)
  
  # 1. 创建映射表 (过滤掉 ID 为 NA 的行)
  valid_dict <- dict[!is.na(dict$FieldID), ]
  id_map <- setNames(valid_dict$Description, as.character(valid_dict$FieldID))
  
  current_names <- names(data)
  
  # 2. 生成新名字
  new_names <- sapply(current_names, function(col_name) {
    # 忽略 eid#' Rename columns from UKB Field IDs to Descriptions
    #'
    #' @param data A data.frame or data.table with raw column names (e.g., p21001_i0).
    #' @param dict The dictionary data frame (e.g., ukb_dict_mini).
    #' @return The data with renamed columns.
    #' @export
    xg_rename <- function(data, dict = ukb_dict_mini) {
      requireNamespace("stringr", quietly = TRUE)
      requireNamespace("data.table", quietly = TRUE)
      
      # 1. 创建映射表 (过滤掉 ID 为 NA 的行)
      valid_dict <- dict[!is.na(dict$FieldID), ]
      id_map <- setNames(valid_dict$Description, as.character(valid_dict$FieldID))
      
      current_names <- names(data)
      
      # 2. 生成新名字
      new_names <- sapply(current_names, function(col_name) {
        # 忽略 eid
        if (tolower(col_name) == "eid") return("Subject_ID")
        
        # 提取 ID (兼容 p21001, f.21001, 21001-)
        col_id <- stringr::str_extract(col_name, "\\d+")
        
        if (is.na(col_id)) return(col_name) # 没数字的列名不改
        
        desc <- id_map[col_id]
        
        if (is.na(desc)) return(col_name) # 字典里没查到的不改
        
        # 提取后缀 (例如 _i0)
        # 正则逻辑：匹配第一个数字之后的所有内容
        suffix <- stringr::str_extract(col_name, "(?<=^.{0,5}\\d).+")
        if (is.na(suffix)) suffix <- ""
        
        # 清洗描述，使其适合做列名 (可选: 如果你喜欢保留空格，可以删掉这几行)
        clean_desc <- gsub("\\s+", "_", desc)          # 空格变下划线
        clean_desc <- gsub("[^[:alnum:]_]", "", clean_desc) # 去掉特殊符号如()
        
        return(paste0(clean_desc, suffix))
      })
      
      # 3. 如果有重复列名 (极其罕见情况)，make.unique 处理
      if (any(duplicated(new_names))) {
        warning("Duplicate descriptions found. Appending numbers.")
        new_names <- make.unique(new_names, sep = "_")
      }
      
      # 4. 修改列名
      dt <- data.table::copy(data)
      data.table::setnames(dt, names(dt), new_names)
      
      return(dt)
    }
    if (tolower(col_name) == "eid") return("Subject_ID")
    
    # 提取 ID (兼容 p21001, f.21001, 21001-)
    col_id <- stringr::str_extract(col_name, "\\d+")
    
    if (is.na(col_id)) return(col_name) # 没数字的列名不改
    
    desc <- id_map[col_id]
    
    if (is.na(desc)) return(col_name) # 字典里没查到的不改
    
    # 提取后缀 (例如 _i0)
    # 正则逻辑：匹配第一个数字之后的所有内容
    suffix <- stringr::str_extract(col_name, "(?<=^.{0,5}\\d).+")
    if (is.na(suffix)) suffix <- ""
    
    # 清洗描述，使其适合做列名 (可选: 如果你喜欢保留空格，可以删掉这几行)
    clean_desc <- gsub("\\s+", "_", desc)          # 空格变下划线
    clean_desc <- gsub("[^[:alnum:]_]", "", clean_desc) # 去掉特殊符号如()
    
    return(paste0(clean_desc, suffix))
  })
  
  # 3. 如果有重复列名 (极其罕见情况)，make.unique 处理
  if (any(duplicated(new_names))) {
    warning("Duplicate descriptions found. Appending numbers.")
    new_names <- make.unique(new_names, sep = "_")
  }
  
  # 4. 修改列名
  dt <- data.table::copy(data)
  data.table::setnames(dt, names(dt), new_names)
  
  return(dt)
}