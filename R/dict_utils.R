#' 搜索变量字典 (Search Variable Dictionary)
#'
#' 根据关键词模糊搜索 UK Biobank 变量字典，无需打开网页版 Showcase。
#'
#' @param keywords 一个或多个字符串关键词 (如 c("sex", "age"))。支持中文或英文（取决于字典内容）。
#' @param view 逻辑值，是否在 View 窗口中展示结果。默认为 TRUE。
#' @return 返回包含 FieldID, Description 等信息的 data.frame
#' @export
#' @examples
#' \dontrun{
#' xg_find("diabetes")
#' xg_find(c("血压", "blood pressure"))
#' }
xg_find <- function(keywords, view = TRUE) {
  # 检查是否有内置字典
  if (!exists("ukb_dict_mini")) {
    stop("Error: 未找到内置字典 `ukb_dict_mini`。请确保包已正确加载。")
  }
  
  # 拼接正则模式 (Case-insensitive)
  pattern <- paste(keywords, collapse = "|")
  
  res <- ukb_dict_mini %>%
    dplyr::filter(stringr::str_detect(Description, stringr::regex(pattern, ignore_case = TRUE)) | 
                  stringr::str_detect(FieldID, pattern))
  
  message(paste0(">>> 共找到 ", nrow(res), " 个相关变量。"))
  
  if (view) {
    View(res)
  }
  return(res)
}

#' 获取指定 Field ID 的完整列名
#'
#' 输入 Field ID (如 "21001")，返回数据集中实际的列名 (如 "f.21001.0.0")
#' @param field_id 字符串或数字
#' @param data_cols 数据集的所有列名向量
#' @return 匹配到的列名向量
#' @export
xg_get_cols <- function(field_id, data_cols) {
  pattern <- paste0("^f\\.", field_id, "\\.")
  grep(pattern, data_cols, value = TRUE)
}