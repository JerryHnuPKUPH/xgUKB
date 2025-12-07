#' Rename columns from UKB Field IDs to Descriptions with smart conflict resolution
#'
#' @param data A data.frame or data.table with raw column names (e.g., p21001_i0).
#' @param dict The dictionary data frame (e.g., ukb_dict_mini).
#' @return The data with renamed columns.
#' @export
xg_rename <- function(data, dict = ukb_dict_mini) {
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' required.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' required.")
  
  # 1. 准备字典
  valid_dict <- dict[!is.na(dict$FieldID), ]
  valid_dict$FieldID <- as.character(valid_dict$FieldID)
  
  # 2. 清洗 Description
  clean_text <- function(x) {
    x <- gsub("[^A-Za-z0-9]", "_", x) # 特殊字符变下划线
    x <- gsub("_+", "_", x)           # 连续下划线合并
    x <- gsub("_$", "", x)            # 去尾
    x <- gsub("^_", "", x)            # 去头
    return(x)
  }
  valid_dict$CleanDesc <- clean_text(valid_dict$Description)
  
  # 3. 冲突检测逻辑
  # 统计描述的重复次数
  desc_counts <- table(valid_dict$CleanDesc)
  duplicate_descs <- names(desc_counts[desc_counts > 1])
  
  # 建立 ID -> CleanDesc 映射
  id_to_desc <- setNames(valid_dict$CleanDesc, valid_dict$FieldID)
  # 建立 ID -> Name (原始变量名, e.g., p21001) 映射，确保 Name 不含 instance 后缀
  # 假设 Name 列是 p21001，如果不确定，这里强制清洗一下只保留 ID 部分的前缀
  # 为了安全，这里只取 FieldID 对应的标准化前缀，比如 "p21001"
  id_to_varname <- setNames(paste0("p", valid_dict$FieldID), valid_dict$FieldID)
  
  # 4. 遍历重命名
  current_names <- names(data)
  
  new_names <- sapply(current_names, function(col_name) {
    # 忽略 Subject_ID / eid
    if (tolower(col_name) %in% c("eid", "subject_id")) return("Subject_ID")
    
    # 提取 ID (匹配如 p28538_i0 中的 28538)
    col_id <- stringr::str_extract(col_name, "\\d+")
    
    # 无效 ID 处理
    if (is.na(col_id) || is.null(id_to_desc[col_id])) return(col_name)
    
    # 获取基础描述
    base_desc <- id_to_desc[col_id]
    
    # 提取后缀 (e.g., _i0, _a1)
    # 逻辑：去除 col_name 里的 "p"+ID (或 "f."+ID) 剩下的就是后缀
    # 比如 p28538_i0 -> 去掉 p28538 -> 剩 _i0
    # 比如 f.28538.0.0 -> 去掉 f.28538 -> 剩 .0.0
    # 正则：匹配 "非数字字符+ID" 的部分并替换为空
    pattern_remove <- paste0("^[^0-9]*", col_id)
    suffix <- sub(pattern_remove, "", col_name)
    
    # --- 核心逻辑修复 ---
    
    # 检查是否有重名冲突
    is_duplicate <- base_desc %in% duplicate_descs
    
    if (is_duplicate) {
      # 如果冲突：使用 "描述_原始ID" + "后缀"
      # new_base = Desc_p28538
      new_base <- paste0(base_desc, "_", id_to_varname[col_id])
    } else {
      # 如果不冲突：使用 "描述"
      new_base <- base_desc
    }
    
    # 拼接后缀
    final_name <- paste0(new_base, suffix)
    
    # 防止双重后缀 (这就是解决 _i0_i0 的关键)
    # 如果 new_base 里已经包含了 suffix (虽然不太可能，但为了健壮性)
    # 或者 suffix 实际上是空的
    # 最重要的是: 确保 id_to_varname 没有带 _i0
    
    return(final_name)
  })
  
  # 5. 应用与防重
  dt <- data.table::copy(data)
  data.table::setnames(dt, names(dt), new_names)
  
  # 假如还是碰巧重名了，最后的防线
  if (any(duplicated(names(dt)))) {
    warning("Duplicate names found after rename. Appending numeric index.")
    data.table::setnames(dt, names(dt), make.unique(names(dt), sep = "_"))
  }
  
  return(dt)
}