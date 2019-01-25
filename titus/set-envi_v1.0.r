## 2019-01-22-SETTING-ENVIRONMENT

rm(list = ls())  # 清除暫存區的資料
# 爬蟲套件安裝（僅第一次需要）
install.packages('xml2')
install.packages('httr')
install.packages('rvest')

# 顯示中文字符（實際施測上沒差，可以考慮不要安裝）
install.packages('tmcn')
install.packages('tm')

# 快捷輸入code的套件，可以用「%>%」這個符號來帶入物件。
install.packages('magrittr')

# 載入套件
library(xml2)
library(httr)
library(tmcn)
library(tm)
library(rvest)
library(magrittr)

message(paste("環境設定完畢。","\n",Sys.time()))
