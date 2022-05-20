
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(ggplot2)
library(RColorBrewer)

library(ggiraph)

#Who won

years <- c(seq(2000,2019),2021)
years <- c(seq(2000,2013),seq(2015,2019),2021)



setwd("~/Sports/baseball/retro/")

files <- paste0("retr_game_",years,".RDS")

retr <- files %>%  map_dfr( readRDS)
setwd("~/Sports/baseball")



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
               
  
  
  
  n_vis_ahead <- gamewinProb %>%   filter( ( (winner == "Vis" & net > 0) |  (winner == "Hm" & net < 0) ) ) %>%
    filter(inning != "9") %>%  mutate(absscore = abs(net)) %>% 
    group_by(inning,absscore)  %>%   summarise( nabs = n(),.groups = "keep") 
  
  
  n_hm_ahead <- gamewinProb %>%   filter( (winner == "Hm" & net > 0) |  (winner == "Vis" & net < 0)  ) %>%
    filter(inning != "9") %>%
    group_by(inning,absscore)  %>%   summarise( nabs = n(),.groups = "keep") 
  
  
  
  
  score_counts <-  gamewinProb %>% filter(net >0 ) %>% 
    group_by(net,inning,winner)  %>% 
    filter(inning != "9") %>%  summarise( n = n(),.groups = "keep") %>%
    mutate(absscore = abs(net)) 
  
  
  
  merged_counts_vs <-  score_counts %>% filter(winner == "Vis") %>% left_join( n_vis_ahead) %>%
    mutate( prob =  n/nabs)  %>%  filter( (net >= 1) & (net <= 4)) %>% 
    mutate( money_line = 100* (-prob/(1 - prob)))  %>% filter( money_line > -1000  ) %>% 
    mutate( net = factor(x = net,levels = seq(4,1) )) 
  
  
  
  merged_counts_hm <-  score_counts %>% filter(winner == "Hm") %>% left_join( n_hm_ahead) %>%
    mutate( prob =  n/nabs)  %>%  filter( (net >= 1) & (net <= 4)) %>% 
    mutate( money_line = 100* (-prob/(1 - prob)))  %>% filter( money_line > -1000  ) %>% 
    mutate( net = factor(x = net,levels = seq(4,1) ))  
  
  
  
  merged_counts <-  rbind(merged_counts_hm ,merged_counts_vs )
  

 
win_plot <-  merged_counts %>% ungroup() %>% mutate(inning = as.numeric(inning) +1) %>% 
                         ggplot( aes(x=inning,y=money_line,color = net,shape=winner)) + geom_line(aes(linetype = winner) ) +
                         geom_point() +  
                 scale_colour_manual(name="Run Advantage",values = brewer.pal(6, "Set1")) +
                        scale_y_continuous(name = 'Money Line', breaks=seq(-1000,0,100),
                        sec.axis = sec_axis(~ (1/ (1 - (100/.))) , breaks=c(seq(.5,.9,.05)),
                                            name = 'Win Probability (%)')
                       ) + scale_x_continuous(name ="Inning", breaks=seq(2,9,1)) + 
                          theme( legend.position = "bottom") +
                   ggtitle(label = "\n Winning Team Wins")


ggsave(filename = "win_plot.pdf",win_plot,device = "pdf",width = 10,height = 7,units = "in")                  
                         
 
   
  
  
  
  
  
  
  
  

