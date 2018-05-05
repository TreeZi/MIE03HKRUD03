#rm(list=ls())
#Games-Credits
library(xml2)
library(rvest)
library(magrittr)
library(httr)
GameID$V1[1]
GameID[1]
GameID = as.matrix(GameID$V1)
data.critic <- data.frame(GameID = NA,
                          Platform = NA,
                          CriticSite = NA,
                          CriticDate = NA,
                          CriticRank = NA,
                          GameLink = NA)
##CREATE DATA FRAME FOR SCRAPED PROXIES
proxy_list = data.frame(x=1,y=2)
colnames(proxy_list) = c("address","port")
##CREATE DATA FRAME FOR CHECKED PROXIES
checked_proxy_list = data.frame(x=1,y=2)
colnames(checked_proxy_list) = c("address","port")
##GET SCRAPED PROXIES
#在第一次用proxy ip抓之前先建立proxyip清單，之後在repeat迴圈就會自動執行
ipdata <- read_html("https://free-proxy-list.net/")
l = 1
for (i in 1:20){
  current_proxy <- html_text(html_nodes(ipdata,xpath=sprintf('//*[@id="proxylisttable"]/tbody/tr[%s]/td[1]', l)))
  current_port <- html_text(html_nodes(ipdata,xpath=sprintf('//*[@id="proxylisttable"]/tbody/tr[%s]/td[2]', l)))
  proxy_list[i,] = c(current_proxy, current_port)
  l= l+1}
for (i in 1:20){
  result = try(GET(url = 'https://httpbin.org/ip',
                   use_proxy(proxy_list[i,1],as.numeric(proxy_list[i,2])))
               ,TRUE)
  ip_ok = grepl("origin", result, fixed=TRUE)
  if(ip_ok == TRUE){
    checked_proxy_list[i,] = c(proxy_list[i,1],as.numeric(proxy_list[i,2]))}}
#CLEAN THE NULL VALUE
checked_proxy_list = checked_proxy_list[!is.na(checked_proxy_list$address),]
checked_proxy_list = checked_proxy_list[checked_proxy_list$address!=1,]
#CREATE THE DATA LIST
ct = NA %>% as.list()
k = 1
repeat{
  if(k > length(GameID)) break
  print(k)
  proxy_number = sample(length(checked_proxy_list[,1]),1)
  X = tryCatch({GET(GameID[k],
                    use_proxy(checked_proxy_list[proxy_number,1],as.numeric(checked_proxy_list[proxy_number,2])))},
               error = function(e) NULL)
  X <- tryCatch({read_html(X)},error = function(e) NULL)
  print(X)
  if (length(X) > 0) {
    print(checked_proxy_list[proxy_number,1])
    gn <- html_nodes(X, '.page-title a') %>% html_text()
    pf <- html_nodes(X, '.core-platform b') %>% html_text()
    cs <- html_nodes(X, '.criticsite') %>% html_text()
    cd <- html_nodes(X, '.cdate') %>% html_text()
    cr <- html_nodes(X, '.criticrating') %>% html_text()
    metaC <- tryCatch(html_nodes(X,'.split') %>% html_text(),error = function(e)NULL)
    if (length(metaC) != 0) {
      metaC <- html_nodes(X,'.split') %>% html_text()
      metaC_link <- html_nodes(X,'.review_link a') %>% html_attr('href')
      metaC = strsplit(metaC[2],split = '\r\n\t\t\t\t\t',fixed = T) %>% unlist()
      metaC = metaC[3] 
      cs <- c(cs,"MetaCritic") 
      cd <- c(cd,metaC_link)
      cr <- c(cr,metaC)
    } 
    if(length(metaC) == 0){
      cs <- NA
      cd <- NA
      cr <- NA
    }
    ct_current = cbind(k,gn,pf,cs,cd,cr,GameID[i])
    # 錯誤處理
    if (length(ct_current) == 7){
      ct[k] = list(ct_current)  
      sprintf('No. %s is done',k) %>% print()
      k <- k + 1
    } else {
      Sys.sleep(60)
      sprintf('No. %s is fail',k) %>% print()
      }
    }
  #每成功執行100筆資料，就重新抓一次proxy ip清單
      if (k %% 100 == 0){
        ipdata <- read_html("https://free-proxy-list.net/")
        l = 1
        for (i in 1:20){
          current_proxy <- html_text(html_nodes(ipdata,xpath=sprintf('//*[@id="proxylisttable"]/tbody/tr[%s]/td[1]', l)))
          current_port <- html_text(html_nodes(ipdata,xpath=sprintf('//*[@id="proxylisttable"]/tbody/tr[%s]/td[2]', l)))
          proxy_list[i,] = c(current_proxy, current_port)
          l= l+1}
        #這裡很厲害，是利用檢查ip的網站回傳檢查proxyip是否有用
        for (i in 1:20){
          result = try(GET(url = 'https://httpbin.org/ip',
                           use_proxy(proxy_list[i,1],as.numeric(proxy_list[i,2])))
                       ,TRUE)
          #try語法怎麼用，use_proxy是甚麼
          ip_ok = grepl("origin", result, fixed=TRUE)
          #grepl怎麼用
          if(ip_ok == TRUE){
            checked_proxy_list[i,] = c(proxy_list[i,1],as.numeric(proxy_list[i,2]))}}
        #刪除空直
        checked_proxy_list = checked_proxy_list[!is.na(checked_proxy_list$address),]
        #這個不懂，應該跟上面那行類似
        checked_proxy_list = checked_proxy_list[checked_proxy_list$address!=1,]
        print('重新抓取Proxy Pool')}
  
}
View(ct)
