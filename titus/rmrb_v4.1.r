## 2019-01-23-rmrb_v4.1
# 抓取人民日報電子檔案，網站連結在下面，有分兩樣：一個是版面，另一個是版面上面的區塊。
# 範圍是從2017/1/1到現在。我覺得最快的抓法是用「動態下載」，但我現在不知道動態下載應
# 該怎麼用才會比較快。傳統的方法應該不大可行，因為從頁面上的特定位置無法抓取到CCS或   
# XPath路徑。其他的待研究。
# 第一要務是先研究如何用R來進行動態下載，並查清楚需要哪些套件的配合。
#                                            ---- 2018-07-24
# 這次從2012年開始下載，下載到2017年。可以研究是不是會有更快的方式。
#                                            ---- 2018-11-23
# 抓取人民日報資料庫的資料，連結下面會附上。
#                                            ---- 2018-12-04
# 目前會遇到網站自動切斷連線，以及封鎖IP位置的問題，因此在設定爬蟲的時間間距時，將時間
# 設定延長，降低被鎖住的機會。
#                                            ---- 2019-01-22
# 將整份code改為輸入年月日即可抓蟲
#                                            ---- 2019-01-23
source("setting-envi.r")

#######################################################################
# 抓取邏輯：
#		(1) 先抓「版面」
#		(2) 再抓「標題」
#		(3) 再抓「內文標題」、「日期」、「內文內容」
#		(4) 切換到下一個「版面」，重複 (2)) ~ (4)
#		(5) 「版面」全部抓完後，切換「下一則新聞」
#		(6) 將抓去下來的內容「存檔」。
#		(7) 重複 (1) ~ (7)。直到最新貼文抓取完畢為止。
#######################################################################
# 
# SETTING FUNCTION
#
## 函式（一）：日期函式 (fun_date)
#  設定從first.date到end.date之間的所有日期；並把這些日期放在固定網域的後面。
fun_date = function(first.date,end.date){
  temp  =  seq.Date(from = as.Date(first.date,format = "%Y%m%d"), by = "day", to = as.Date(end.date,format = "%Y%m%d")) 
  temp  =  gsub("-","",temp)
  link = paste0('http://data.people.com.cn/rmrb/',temp,"/")
  return(link)}

## 函式（二）：版面函式（fun_page）
#  取得每一天新聞的所有版面，並將所有版面的連結存成「pagelist」，最後將pagelist
#  匯出（return）。
fun_page = function(link){
  print(sprintf("================ 抓取網頁： %s ================",link))
  link_withall = paste0(link,"1?code=1")
  #  「read_html(httr::GET(...,timeout(s)))」因為只有在GET函式裡面才能使用
  #  「timeout」函數。「timeout」的用法是，如果超過指定的秒數，就會自動判讀讀取網
  #   頁失敗。「encoding ' "UTF-8"'」表示網頁編碼使用UTF-8，使R在讀取中文網頁
  #   時，不會有亂碼。
  #   「tryCatch」若出現錯誤，則回傳「NA」值；並打印錯誤訊息。
  html = tryCatch(read_html(httr::GET(link_withall, timeout(10)),
			    encoding = "UTF-8", options = 'HUGE'),
		  error = function(msg){
                    message(paste(msg,Sys.time(),sep = '\n'))
                    return(NA)},
                  warning = function(msg){
                    message(paste(msg,Sys.time(),sep = "\n"))
                    return(NA)})
  pagelist = tryCatch(html_nodes(html,"#UseRmrbPageNum") %>% html_text(),
                      error = function(msg){
                        message(paste(msg,Sys.time(),sep = "\n"))
                        return(NA)},
                      warning = function(msg){
                        message(paste(msg,Sys.time(),sep = "\n"))
                        return(NA)})
  pagelist = c(1:as.numeric(pagelist))
  print(paste0("finish at ",Sys.time()))
  Sys.sleep(runif(2,3,6))
  return(list(pagelist))}


## 函式（三）：標題函式（fun_title）
#  取得每一版上的標題連結，並將所有標題連結存成「titlelist」。最後將titlelist
#  匯出（return）。

fun_title = function(link,page){
  link_withpage = paste0(link,"/",page)  
  # 這邊的寫法同上。
  html_page = tryCatch(read_html(httr::GET(link_withpage, timeout(10)),encoding = "UTF-8",options = 'HUGE'),
                       error = function(msg){
                         message(paste(msg,Sys.time(),sep = "\n"))
                         return(NA)},
                       warning = function(msg){
                         message(paste(msg,Sys.time(),sep = "\n"))
                         return(NA)})
  titlelist = tryCatch(html_nodes(html_page,".title_list a") %>% html_attr("href"),
                       error = function(msg){
                         message(paste(msg,Sys.time(),sep = "\n"))
                         return(NA)},
                       warning = function(msg){
                         message(paste(msg,Sys.time(),sep = "\n"))
                         return(NA)})
  print(paste0("finish at ",Sys.time()))  
  return(list(titlelist))}

## 函式（四）：內文函式（fun_arti）
#  從標題頁面中抓取：標題、日期、與內文，並存成「arti」。

fun_arti = function(title){
  link_withtitle = paste0('http://data.people.com.cn',title)
  html_title = tryCatch(read_html(httr::GET(link_withtitle, timeout(10)),encoding = "UTF-8",options = 'HUGE'),
                        error = function(msg){
                          message(paste(msg,Sys.time(),sep = "\n"))
                          return(NA)},
                        warning = function(msg){
                          message(paste(msg,Sys.time(),sep = "\n"))
                          return(NA)})
  if (is.na(html_title) == TRUE) {
    title = NA
    date = NA
    ctnt = NA
    print("抓取網頁失敗！")} else {
      title = html_nodes(html_title,'.title') %>% html_text()     #內文標題的CSS
      if (length(title)==0) title = NA
      author = html_nodes(html_title,'.author') %>% html_text()   #文章作者的CSS
      if (length(author)==0) author = NA
      ctnt = html_nodes(html_title,'#FontZoom') %>% html_text()   #內文內容的CSS
      if (length(ctnt)==0) ctnt = NA}   
  Sys.sleep(runif(2,15,30))
  arti = list(title,author,ctnt)
  message(title)
  message(paste0("finish at ",Sys.time()))     
  return(arti)}

## 函式（五）：爬蟲與寫入函式（fun_crawlandwrite）
fun_crawlandwrite = function(styr,mnth,stdy){
	MM = c("01",'02','03','04','05','06','07','08','09','10','11','12')
	if ( mnth < 12 ){
		 period = seq.Date(from = as.Date(paste0(styr,MM[mnth],'01'),format = "%Y%m%d"), by = "day", to = as.Date(paste0(styr,MM[mnth+1],'01'),format = "%Y%m%d"))
		 first.date = period[as.numeric(stdy)]
		 end.date = period[length(period)-1]
	 }
	if (mnth == 12 ){
		period = seq.Date(from = as.Date(paste0(styr,MM[mnth],'01'),format = "%Y%m%d"), by = "day", to = as.Date(paste0(styr+1,MM[1],'01'),format = "%Y%m%d"))
		first.date = period[as.numeric(stdy)]
		end.date = period[length(period)-1]	
	}
	print(paste(first.date,end.date))
	link = fun_date(first.date,end.date)
	date.length = seq.Date(from = as.Date(first.date,format = "%Y%m%d"), by = "day", to = as.Date(end.date,format = "%Y%m%d")) %>% length()
	A = lapply(link[1:date.length],fun_page)
	DATE  =  seq.Date(from = as.Date(first.date,format = "%Y%m%d"), by = "day", to = as.Date(end.date,format = "%Y%m%d"))  # 另外設置一個DATE資料，以便最後進行資料整併時，作為其中一項變數資料。
  
  for (seed in 1:length(A)){
    print(sprintf("================ 抓取 %s 的報紙 ================",DATE[seed]))
    B = unlist(A[seed])
    E = matrix(ncol = 6)              # 設定一個矩陣，寬度為6
    colnames(E) = c(                  # 矩陣的標題分別為這六項。
      "NewsDate",       # 是哪一天的新聞。
      "Page",           # 是第幾版。
      "Title",          # 標題為何。
      "Author",         # 內文的作者。
      "Content",        # 內文內容。
      "URL")            # 該篇文章的連結。
    for (pageseed in 1:length(B)){
      print(sprintf("第 %d 版的內容",pageseed))
      C = fun_title(link[seed],B[pageseed])
      D = unlist(C)
      for (titleseed in 1:length(D)){
        message(paste0("No. ",titleseed,":"))
        E_tmp = list(as.character(DATE[seed]),as.character(pageseed),unlist(fun_arti(D[titleseed])),D[titleseed])    # 把新聞日期、新聞版面、新聞內容（共三筆資料）、文章連結存成list「E_tmp」
        E = rbind(E,unlist(E_tmp))                          # 用指令「rbind」把「E_tmp」資料併在「E」下面（上下列合併）
        Sys.sleep(runif(2,15,30))}}                                      # 跑2輪，暫停2 ~ 4秒，以免被鎖IP
    E = E[-1,]                                              # 將矩陣第一列的資料（為六項NA值）移除
    filename = paste0(DATE[seed],".csv")                    # 設定檔案名稱「filename」
    write.csv(E,file = filename,fileEncoding = "UTF-8")     # 把矩陣「E」匯出成CSV檔，檔案名稱為「日期.csv」
    # 在匯出成CSV檔後，會在新一輪的Loot中，重製矩陣「E」。這樣就可以確保每一份CSV檔都是那一天的新聞。
    print(paste0("================ ","FINISH and write into ",filename,"================"))}}

######################################################################################
# 
# RUNNING PROGRAM
## 函式（六）：運行程式（RUNNING PROGRAM）
RUN_FUN = function(styr,mnth,stdy,endy){
		repeat{
				if (styr > endy) break
				repeat{
						if ( mnth < 12 ){
								fun_crawlandwrite(styr,mnth,stdy)
								Sys.sleep(7200)
								message("========休息個2小時，看你機器人還抓不抓得到我^___^========")
								mnth = mnth + 1
						}
						if (mnth == 12 ){
								fun_crawlandwrite(styr,mnth,stdy)
								Sys.sleep(7200)
								message("========休息個2小時，看你機器人還抓不抓得到我^___^========")
						}
						if (mnth == 12) break
						stdy = '01'

				}
				mnth = 1
				styr = styr + 1
				Sys.sleep(7200)
				message("========休息個2小時，看你機器人還抓不抓得到我^___^========")
		}
		print(paste("抓到",endy,"，停止抓取。"))
}


