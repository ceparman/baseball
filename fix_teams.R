library(dplyr)
library(readxl)
library(xlsx)
library(stringr)
library(lubridate)


#get all team codes from odds data

teams <- character()


for(y in 2015:2021){
  
  
  file <- paste0("hist_odds/mlbodds-",y,"-mod.xlsx")
  odds <-    read_excel(file)

  teams <- c(teams,unique(t$Team))

  
}

odds_teams <- unique(teams)

write.csv(odds_teams,"odds_teams.csv")


#get all teams form retro

teams <- character()
for(y in c(2015:2019,2021)){
  
  ret <- readRDS( paste0("retro/retr_game_",y,".RDS")  )
  
 teams <- c(teams,(unique(ret$VisTm)))
    
 
}
 

retro_teams <- unique(teams)

write.csv(retro_teams,"retro_teams.csv")


#Create lookup table


miss_teams_raw <- union(retro_teams,odds_teams)[ !( union(retro_teams,odds_teams) %in% intersect(retro_teams,odds_teams))]

write.csv(miss_teams_raw,"miss_teams_raw.csv")

common_teams <- intersect(retro_teams,odds_teams)

#Create odds data set


miss_teams <- read.csv("miss_teams'.csv")

miss_teams$X <- NULL


all_teams <- rbind(miss_teams,data.frame(retro=common_teams,odds=common_teams))


write.csv(all_teams,"all_teams.csv")


#Update odds files

all_teams <-read.csv("all_teams.csv")
odds_teams <-read.csv("odds_teams.csv")

for(y in 2015:2021){

  
  file <- paste0("hist_odds/mlbodds-",y,".xlsx")
  odds <-    read_excel(file,na = c("x","NL","-"))
  
  
  #fix columns  
  odds <- odds %>%  rename( c( RunsMoney = `...19`, openOLmoney =`...21`,closeOLmoney = `...23` ))

  
  odds$`9th` <- as.numeric(as.character(odds$`9th` ))
  
 print( table(odds$Team %in% all_teams$odds))

 odds$new_Date <- rep(as.Date("1-1-2000"),nrow(odds))

#update rows in file

for(i in 1:nrow(odds)){
  
# update team to retro team
  odds$Team[i] <-  all_teams$retro[ which(all_teams$odds == odds$Team[i])]
  
  
#make date real  data 
  
str_date <- as.character(odds$Date[i])  
  
day <-   str_sub( str_date, - 2, - 1)
month <- str_sub( str_date, 1, str_length(str_date)-2)

ymd(paste(y,month,day,sep="-"))


odds$new_Date[i] <- as.Date(ymd(paste(y,month,day,sep="-")))
  
}


 odds <- as.data.frame(odds) %>% select(-Date) %>% rename(Date = new_Date)
 


xlsx::write.xlsx(odds,paste0("hist_odds/mlbodds-",y,"-mod.xlsx"),row.names = F)

}
