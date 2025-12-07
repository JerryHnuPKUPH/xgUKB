#' Search for fields in the UKB dictionary
#'
#' This function searches across multiple columns (Name, Description, FieldID, Entity)
#' to find relevant variables. It is case-insensitive.
#'
#' @param keyword A string to search for (e.g., "BMI", "21001", "YTHDF3").
#' @param data The dictionary data frame to search in. Default is `ukb_dict_mini`.
#' @return A data.table or data.frame containing matching rows.
#' @export
xg_find <- function(keyword, data = ukb_dict_mini) {
  # 1. 输入检查
  if (missing(keyword) || is.null(keyword) || keyword == "") {
    stop("Please provide a keyword to search (e.g., xg_find('BMI')).")
  }
  
  if (!exists("ukb_dict_mini") && missing(data)) {
    stop("Data dictionary 'ukb_dict_mini' not found. Please load it first.")
  }
  
  # 转换 keyword 为字符，防止传入数字报错
  pattern <- as.character(keyword)
  
  message(paste0(">>> Searching for '", pattern, "' in dictionary..."))
  
  # 2. 定义我们要搜索的“核心列”
  # 我们希望在这些列里只要有一个匹配上就算成功
  search_cols <- c("Name", "FieldID", "Description", "Entity", "Type")
  
  # 确保这些列在数据中真的存在
  valid_cols <- intersect(names(data), search_cols)
  
  if (length(valid_cols) == 0) {
    stop("The dictionary data does not have standard columns (Name, Description, FieldID).")
  }
  
  # 3. 执行全字段搜索
  # 初始化一个全为 FALSE 的向量
  matches <- rep(FALSE, nrow(data))
  
  for (col in valid_cols) {
    # 将该列转换为字符（处理 FieldID 为数字的情况，或 NA 的情况）
    col_values <- as.character(data[[col]])
    
    # 查找匹配 (ignore.case = TRUE 是关键，能匹配 ythdf3 和 YTHDF3)
    # 使用 grepl 返回逻辑向量
    found_in_col <- grepl(pattern, col_values, ignore.case = TRUE)
    
    # 逻辑或 (OR)：只要之前的列匹配了，或者这一列匹配了，都算
    matches <- matches | found_in_col
  }
  
  # 4. 提取结果
  result <- data[matches, ]
  
  # 5. 结果反馈
  if (nrow(result) == 0) {
    message("No matches found.")
    return(NULL)
  } else {
    message(paste(">>> Found", nrow(result), "matches."))
    # 移除行名，让显示更整洁
    rownames(result) <- NULL
    return(result)
  }
}