#' Search for keywords in the dictionary
#'
#' @param keyword A character vector or string to search (e.g., "BMI" or c("BMI", "Age")).
#' @param dict The dictionary data to search in (default: ukb_dict_mini).
#' @return A data frame containing the matching rows.
#' @export
xg_find <- function(keyword, dict = ukb_dict_mini) {
  
  # 1. 检查 keyword 参数是否有效
  if (missing(keyword) || is.null(keyword) || length(keyword) == 0) {
    stop("Please provide a keyword to search.")
  }
  
  # 过滤掉空字符串，防止搜出所有东西
  keyword <- keyword[keyword != ""]
  if (length(keyword) == 0) {
    stop("Keywords contain only empty strings.")
  }
  
  # 2. 检查字典格式
  required_cols <- c("Name", "Description", "FieldID")
  if (!all(required_cols %in% names(dict))) {
    stop("The dictionary data does not have standard columns (Name, Description, FieldID).")
  }
  
  # 3. 构建多词搜索逻辑
  # 将 c("BMI", "center") 转换为正则表达式 "BMI|center"
  # | 代表“或者”，意思是只要匹配其中任何一个词就把它找出来
  search_pattern <- paste(keyword, collapse = "|")
  
  message(paste0(">>> Searching for patterns: '", search_pattern, "'"))
  
  # 4. 执行搜索 (不区分大小写)
  # 在 Description 中搜索 OR 在 FieldID 中搜索
  matches_desc <- grep(search_pattern, dict$Description, ignore.case = TRUE)
  matches_id   <- grep(search_pattern, as.character(dict$FieldID), ignore.case = TRUE)
  
  # 取并集 (unique)
  match_indices <- unique(c(matches_desc, matches_id))
  
  # 5. 返回结果
  result <- dict[match_indices, ]
  
  if (nrow(result) == 0) {
    message("No results found.")
    return(NULL)
  }
  
  return(result)
}