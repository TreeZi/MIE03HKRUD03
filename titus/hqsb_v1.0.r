# 2019-01-25-TITUS
# 抓取還球時報的電子檔，時間從2012-01-01 到 2018-11-30。
# 											---- 2019-01-25
#
# 
rm(list = ls())  # 清除暫存區的資料
install.packages('xml2')
install.packages('httr')
install.packages('tmcn')
install.packages('tm')
install.packages('rvest')
install.packages('magrittr')
library(xml2)
library(httr)
library(tmcn)
library(tm)
library(rvest)
library(magrittr)
# http://data.people.com.cn/pd/hqsb/s

fun_Num = function(pageNo){
  srch.page = paste0('http://data.people.com.cn/pd/hqsb/s?top=0&pageNo=',pageNo,'&qs=%7B%22cId%22%3A%2263%22%2C%22cds%22%3A%5B%7B%22fld%22%3A%22dataTime.start%22%2C%22cdr%22%3A%22AND%22%2C%22hlt%22%3A%22false%22%2C%22vlr%22%3A%22AND%22%2C%22qtp%22%3A%22DEF%22%2C%22val%22%3A%222012-01-01%22%7D%2C%7B%22fld%22%3A%22dataTime.end%22%2C%22cdr%22%3A%22AND%22%2C%22hlt%22%3A%22false%22%2C%22vlr%22%3A%22AND%22%2C%22qtp%22%3A%22DEF%22%2C%22val%22%3A%222018-11-30%22%7D%5D%2C%22obs%22%3A%5B%7B%22fld%22%3A%22dataTime%22%2C%22drt%22%3A%22DESC%22%7D%5D%7D')
  page_ht = tryCatch(read_html(httr::GET(srch.page, timeout(15)),
                            encoding = "UTF-8"),
                  error = function(msg){
                    message(paste(msg,Sys.time(),sep = '\n'))
                    return(NA)},
                  warning = function(msg){
                    message(paste(msg,Sys.time(),sep = "\n"))
                    return(NA)})
  artilink = tryCatch(html_nodes(page_ht,"h2 a") %>% html_attr("href"),
                      error = function(msg){
                        message(paste(msg,Sys.time(),sep = "\n"))
                        return(NA)},
                      warning = function(msg){
                        message(paste(msg,Sys.time(),sep = "\n"))
                        return(NA)})
  date = tryCatch(html_nodes(page_ht,".news_sum_bottom span") %>% html_text(),
                  error = function(msg){
                    message(paste(msg,Sys.time(),sep = "\n"))
                    return(NA)},
                  warning = function(msg){
                    message(paste(msg,Sys.time(),sep = "\n"))
                    return(NA)})
  date = gsub("时间： ","",date)
  print(paste0("finish at ",Sys.time()))
  return(list(list(artilink),list(date)))}
fun_arti =function(artilink){
  arti_ht = tryCatch(read_html(httr::GET(paste0('http://data.people.com.cn',artilink), timeout(15)),
                                 encoding = 'UTF-8', options = 'HUGE'),
                       error = function(msg){
                         message(paste(msg,Sys.time(),sep = '\n'))
                         return(NA)},
                       warning = function(msg){
                         message(paste(msg,Sys.time(),sep = "\n"))
                         return(NA)})
  if (is.na(arti_ht) == TRUE) {
    title = NA
    ctnt = NA
    print("抓取網頁失敗！")} else {
      title = html_nodes(arti_ht,".title h2") %>% html_text()     #內文標題的CSS
      if (length(title)==0) title = NA
      ctnt = html_nodes(arti_ht,"#detail-p") %>% html_text()
      if (length(ctnt)==0) ctnt = NA}
  arti = list(title,ctnt)
  message(title)
  message(paste0("finish at ",Sys.time()))
  Sys.sleep(runif(2,15,30))
  return(arti)}


FUN_RUN = function(ST,EndNO){
  for (pageNo in ST:EndNo){
    print(sprintf("================ 抓取 %s 的內容 ================",pageNo))
    C = fun_Num(pageNo)
    D = unlist(C)[1:(length(unlist(C))/2)]
    TT = unlist(C)[((length(unlist(C))/2)+1):length(unlist(C))]
    A = lapply(D[1:length(D)],fun_arti)
    E = matrix(ncol = 4)              # 設定一個矩陣，寬度為6
    colnames(E) = c(                  # 矩陣的標題分別為這六項。
      "NewsDate",       # 是哪一天的新聞。
      "Title",          # 標題為何。
      "Content",        # 內文內容。
      "URL")            # 該篇文章的連結。
    for (artiseed in 1:length(A)){
      check_date = TT[artiseed]
      E_tmp = c(TT[artiseed],unlist(A[artiseed][1]),unlist(A[artiseed][2]),D[artiseed])
      E = rbind(E,E_tmp)}
    E = E[-1,]                                              # 將矩陣第一列的資料（為六項NA值）移除
    filename = paste0('hqsb','_No_',pageNo,".csv")                    # 設定檔案名稱「filename」
    write.csv(E,file = filename,fileEncoding = "UTF-8")     # 把矩陣「E」匯出成CSV檔，檔案名稱為「日期.csv」
    print(paste0("================ ","FINISH and write into ",filename,"================"))
    Sys.sleep(runif(2,15,30))}}

FUN_RUN()


print('5610是最後一頁')





