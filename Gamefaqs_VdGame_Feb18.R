#Gamefaqs_VdGame_Feb09----------------------------------
library(xml2)
library(rvest)
library(magrittr)
#j <- 1
#list <- NULL
#改網址-----------------------------------------
#vd.list.crdt.re <- lapply(1:length(vd.list.crdt),function(i){sub("https://www.gamefaqs.com","https://gamefaqs.gamespot.com",vd.list.crdt[i])})
#vd.list.crdt.re <- unlist(vd.list.crdt.re)

repeat{
  if(j > length(vd.list.crdt.re)) break
  x <- read_html(vd.list.crdt.re[j])
  crdt <- html_nodes(x,".credits a") %>% html_text()
  vd <- html_nodes(x,".page-title a") %>% html_text()
  if (length(crdt)==0)  list.tmp <- cbind(vd,"NA")
  if (length(crdt) >0)  list.tmp <- cbind(vd,crdt)
  list[j] <- list(list.tmp)
  j <- j + 1
  Sys.sleep(runif(2,2,4))
  if (j %% 200 == 0) save.image("路徑")
}
      save.image("路徑")
process <- j
#轉成table---------------------------------------------------
table <- NULL
for(i in 1:length(list)){
table.tmp <- matrix(unlist(list[i]), nrow = length(unlist(list[i]))/2 , ncol = 2, byrow = F)
table <- rbind(table,table.tmp)
}
table <- as.data.frame (table)



