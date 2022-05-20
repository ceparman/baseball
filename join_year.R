lib


visitor <-odds  %>% select(Team,Open,Close,`Run Line`,RunsMoney,`Open OU`,openOLmoney,`Close OU`,closeOLmoney,Date)  %>%
                    rename_all( funs(paste0(.,"_VIS"))) %>% 
                  right_join(retr_game_2015,by = c("Date_VIS" = "Date","Team_VIS" = "VisTm")) %>%
                  rename(Date = Date_VIS)
                         
                         
home <-odds  %>% select(Team,Open,Close,`Run Line`,RunsMoney,openOLmoney,closeOLmoney,Date)  %>%
  rename_all( funs(paste0(.,"_HOM"))) %>% 
  right_join(retr_game_2015,by = c("Date_HOM" = "Date","Team_HOM" = "HmTm"))  %>%
  rename(Date = Date_HOM) %>% select(1:9)
                         
                         
 full_set <- full_join(visitor,home, by = c("Date" ="Date","HmTm" = "Team_HOM"))
                         
                         
        

home <- merge(retr_schedule_2015,odds, by.x=c("Date","Team"),by.y=c("Date","HmTeam"))


complete_game <- merge(visitor,home, by.x=c("Date","Team","HmTeam"),by.y=c("Date","VisTeam","Team"))
                       