#' 快速读取并清洗 UKB 数据 (Quick Load & Clean)
#'
#' 从大型 CSV/Tab 文件中提取指定变量，并自动整理格式。
#'
#' @param file_path 本地 UKB 数据文件路径 (.csv, .tab, .txt)。
#' @param field_ids 需要提取的 Field ID 向量 (如 c("31", "21001"))。
#' @param eid_col 主键列名，默认为 "f.eid"。
#' @return 清洗后的 data.frame (tibble)
#' @export
xg_extract <- function(file_path, field_ids, eid_col = "f.eid") {
  requireNamespace("data.table")
  
  message(">>> 正在扫描文件表头...")
  # 只读取第一行获取列名
  all_headers <- names(data.table::fread(file_path, nrows = 0))
  
  # 构造要提取的列名列表
  # 始终包含 eid
  target_cols <- c(eid_col)
  
  for (fid in field_ids) {
    # 匹配 f.XXXX.x.x 格式
    found <- grep(paste0("^f\\.", fid, "\\."), all_headers, value = TRUE)
    if (length(found) > 0) {
      target_cols <- c(target_cols, found)
    } else {
      warning(paste("未在文件中找到 Field ID:", fid))
    }
  }
  
  target_cols <- unique(target_cols)
  
  message(paste0(">>> 正在提取 ", length(target_cols), " 列数据 (依赖 data.table，速度较快)..."))
  
  # 使用 fread 的 select 参数只读特定列，极大节省内存
  dt <- data.table::fread(file_path, select = target_cols)
  
  # 转换为 tibble 方便后续处理
  df <- dplyr::as_tibble(dt)
  
  message(">>> 提取完成！")
  return(df)
}

#' 自动合并基线与随访数据 (Auto Merge Instances)
#' 
#' 将 f.xxxx.0.0 (基线), f.xxxx.1.0 (第一次随访) 等合并为一列，或取均值。
#' 该逻辑模拟 ukhelp 的自动清洗。
#' 
#' @param df 数据框
#' @param field_id 目标 Field ID
#' @param method 合并方式: "first" (优先取基线，缺失则取随访), "mean" (取均值)
#' @export
xg_merge_instances <- function(df, field_id, method = "first") {
  # 找到该 field 对应的所有列
  cols <- grep(paste0("^f\\.", field_id, "\\."), names(df), value = TRUE)
  
  if (length(cols) == 0) return(NULL)
  
  # 简单实现：取第一非空值 (Coalesce)
  if (method == "first") {
    # 动态构造 coalesce 调用
    res_vec <- dplyr::coalesce(!!!dplyr::select(df, dplyr::all_of(cols)))
    return(res_vec)
  }
  
  # 取行均值
  if (method == "mean") {
    res_vec <- rowMeans(dplyr::select(df, dplyr::all_of(cols)), na.rm = TRUE)
    # 将 NaN 转回 NA
    res_vec[is.nan(res_vec)] <- NA
    return(res_vec)
  }
}