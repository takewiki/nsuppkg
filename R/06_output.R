


#' 处理结果的输出方式
#'
#' @param brand 品牌
#' @param data 传入数据
#' @param each_page 每页数
#' @param skip_row  行数
#' @param file_name  文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_output_download();
nsim_output_download <- function(brand ='JBLH',data,each_page=7000L,skip_row=2L){
  data_template <-tsda::nsim_data_tpl()
  res_formatted <- data
  res <- rbind(data_template,res_formatted);
  res <- tsdo::df_paging(res,each_page,skip_row);
  return(res)

}

#' 处理结果最终结构
#'
#' @param brand 品牌
#' @param each_page 页数
#' @param skip_row 忽略行
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_output_download_nscs();
nsim_output_download_nscs <- function(brand ='JBLH',each_page=7000L,skip_row=2L){
  data <- nscspkg::nsim_nscs_current(brand);
  res <-nsim_output_download(brand,data,each_page,skip_row);
  return(res);
}



#' 处理结果的输出方式
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_output_download();
nsim_output_download_test <- function(brand ='JBLH',each_page=3000L,skip_row=2L,file_name='output.res.paging.20191023T.xlsx'){
  data_template <-tsda::nsim_data_tpl()
  res <- nsim_output(brand);
  row_count <- nrow(res);
  A <- paste("捷豹路虎",tsdo::left(as.character(Sys.Date()),7),'未分类',sep="/");
  B <- res$FQues_std;
  C <- res$FQues_like;
  D <- res$FAnsw_std;
  E <- rep("1",row_count);
  F <-rep("",row_count);
  G <-rep("",row_count);
  H <- rep("1",row_count);
  I <-rep("",row_count);
  res_formatted <- data.frame(A,B,C,D,E,F,G,H,I,stringsAsFactors = FALSE);
  res <- rbind(data_template,res_formatted);
  res <- tsdo::df_paging(res,each_page,skip_row);
  openxlsx::write.xlsx(res,file_name)

}

