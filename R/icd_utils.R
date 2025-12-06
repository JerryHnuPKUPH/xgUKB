#' 基于 ICD-10 定义疾病 (Define Disease by ICD-10)
#'
#' 输入 ICD 前缀，自动从主诊断和副诊断列 (f.41270) 中筛选确诊患者。
#'
#' @param df 已加载的数据框 (必须包含 f.41270 系列列)。
#' @param icd_codes 字符串向量，如 c("I20", "I25")。
#' @param strict 逻辑值。TRUE=精确匹配, FALSE=前缀匹配(默认)。
#' @return 返回一个由 0/1 组成的向量，1表示患病。
#' @export
xg_define_disease <- function(df, icd_codes, strict = FALSE) {
  # 找到所有 ICD10 诊断列 (f.41270.x.x)
  icd_cols <- grep("^f\\.41270\\.", names(df), value = TRUE)
  
  if (length(icd_cols) == 0) {
    stop("Error: 数据框中未找到 f.41270 (ICD-10 Diagnoses) 相关列。")
  }
  
  message(">>> 正在扫描 ICD-10 记录...")
  
  # 将数据转为长格式以便搜索 (这种方式比对每一列做 grep 更稳健)
  # 注意：大数据量下这步可能较慢，这里采用简化的 apply 逻辑
  
  # 创建一个正则模式
  if (strict) {
    pattern <- paste0("^(", paste(icd_codes, collapse = "|"), ")$")
  } else {
    # 前缀匹配，如 I25 会匹配 I25.1, I25.9
    pattern <- paste0("^(", paste(icd_codes, collapse = "|"))
  }
  
  # 定义一个内部函数：判断一行是否包含 pattern
  check_row <- function(row_data) {
    any(stringr::str_detect(na.omit(as.character(row_data)), pattern))
  }
  
  # 对 ICD 列进行行遍历
  # 这里的 df[icd_cols] 需要是 data.frame 或 matrix
  has_disease <- apply(df[, icd_cols], 1, check_row)
  
  return(as.integer(has_disease))
}