

#gets data for game odds and game stats.
#2020 game data failing

library(httr)

setwd("hist_odds/")
for(y in 2015:2021){
  
  
  file <- URLencode(  paste0("https://www.sportsbookreviewsonline.com/scoresoddsarchives/mlb/mlb odds ",y,".xlsx") )
  
  
  URLdecode(file)
download.file(url = file,destfile = paste0("mlbodds-",y,".xlsx"))
}


setwd("../")
setwd("retro/")
for(y in 2000:2015){
  
for (t in c("schedule","roster","game")  ) {
  
 x <-  get_retrosheet(t, y)  
  
assign(  paste0("retr_",t,"_",y),x )  
  
saveRDS(get(paste0("retr_",t,"_",y)),  paste0("retr_",t,"_",y,".RDS"))  

}

  
}
setwd("../")



