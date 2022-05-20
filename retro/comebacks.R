
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(ggplot2)

#Who won

retr <- rbind( readRDS("retr_game_2015.RDS"),
               readRDS("retr_game_2016.RDS"),
               readRDS("retr_game_2017.RDS"),
               readRDS("retr_game_2018.RDS"),
               readRDS("retr_game_2019.RDS"),
               readRDS("retr_game_2021.RDS")

               )


retr <- retr %>%   mutate(winner =ifelse(VisRuns > HmRuns, "Vis","Hm" )) %>%
                    rownames_to_column(var = "gameId") %>%  
                    select(gameId,winner,HmLine,VisLine)
    

retr <-  retr %>%   mutate( HMi1 =  as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][1])),   
                        HMi2  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][2])),
                        HMi3  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][3])),
                        HMi4  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][4])),
                        HMi5  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][5])),
                        HMi6  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][6])),
                        HMi7  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][7])),
                        HMi8  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][8])),
                        HMi9  = as.numeric(  map_chr(HmLine, ~ str_split(.,"")[[1]][9]))
                        ) %>%
              mutate(   Visi1 =  as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][1] )),   
                        Visi2  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][2])),
                        Visi3  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][3])),
                        Visi4  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][4])),
                        Visi5  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][5])),
                        Visi6  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][6])),
                        Visi7  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][7])),
                        Visi8  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][8])),
                        Visi9  = as.numeric(  map_chr(VisLine, ~ str_split(.,"")[[1]][9]))
              ) %>%
  
  
             mutate( netScore1 = ifelse ( winner == "Vis", Visi1 - HMi1,HMi1- Visi1),
                     netScore2= ifelse ( winner == "Vis",  Visi2 - HMi2,HMi2- Visi2)+ netScore1,
                     netScore3 = ifelse ( winner == "Vis", Visi3 - HMi3,HMi3- Visi3)+ netScore2,
                     netScore4 = ifelse ( winner == "Vis", Visi4 - HMi4,HMi4- Visi4)+ netScore3,
                     netScore5 = ifelse ( winner == "Vis", Visi5 - HMi5,HMi5- Visi5)+ netScore4,
                     netScore6 = ifelse ( winner == "Vis", Visi6 - HMi6,HMi6- Visi6)+ netScore5,
                     netScore7 = ifelse ( winner == "Vis", Visi7 - HMi7,HMi7- Visi7)+ netScore6,
                     netScore8 = ifelse ( winner == "Vis", Visi8 - HMi8,HMi8- Visi8)+ netScore7,
                     netScore9 = ifelse ( winner == "Vis", Visi9 - HMi9,HMi9- Visi9)+ netScore8
             )
                     
                     
                     
  gamewinProb <-  retr %>% select( c( gameId,winner,contains("net")))    %>%
    
                  tidyr::gather("inning","net",3:11) %>%
                  mutate(inning = str_sub(inning,9,9)) %>%
                  mutate(absscore = abs(net))
                     
 score_abs_counts <-  gamewinProb %>% group_by(absscore,inning) %>%  filter( !is.na(absscore)) %>%
                  filter(inning != "9") %>%  summarise( nabs = n()) %>% select(absscore, inning,nabs) 

 
 score_counts <-  gamewinProb %>% group_by(net,inning) %>%  filter( !is.na(net)) %>%
              filter(inning != "9") %>%  summarise( n = n()) %>%  mutate(absscore = abs(net)) %>%  select(net, inning,n,absscore )  
 
 
 merged_counts <-  score_counts %>% left_join(score_abs_counts ) %>% mutate( prob =  n/nabs) %>%
  filter( (net >= -4) & (net <= 6)) %>% mutate( net = as.factor(net)) %>% filter( prob < .5 & prob > .05)  %>%
   mutate( money_line = -100* ((1 -prob)/(prob)) )  
    

 
 
 merged_counts %>% ungroup() %>% mutate(inning = as.numeric(inning) +1) %>% 
                         ggplot( aes(x=inning,y=money_line,color = net)) + geom_line() +
                         geom_point() +  
                 scale_colour_manual(name="Run Deficit",values = c("orange","purple","red","green")) +
                        scale_y_continuous(name = 'Money Line', breaks=seq(-2000,0,200),
                        sec.axis = sec_axis(~ ( 100/(1- ./100)) , breaks=seq(4,24,2),
                                            name = 'Win Probability (%)')
                       ) + scale_x_continuous(name ="Inning", breaks=seq(2,9,1)) + 
                          theme( legend.position = "bottom")
                  
                         
 
   
  
  
  
  
  
  
  
  

