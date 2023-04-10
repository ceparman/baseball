

get_standings <- function()
{


tab <- rvest::read_html("https://www.espn.com/mlb/standings") |> rvest::html_table()

rows1 <- !tab[[1]]$X1 %in% c("East","Central","West")

al <- cbind( tab[[1]][rows1,], tab[[2]][rows1,])


rows2 <- !tab[[3]]$X1 %in% c("East","Central","West")

nl <- cbind( tab[[3]][rows2,], tab[[4]][rows2,])

standings <-  rbind(al,nl)
colnames(standings) <- unlist(c("team",tab[[2]][1,]))

team_df <- data.frame(
  
team = factor( c(
"TBTampa Bay Rays",         "NYYNew York Yankees",      "TORToronto Blue Jays",    
 "BOSBoston Red Sox",        "BALBaltimore Orioles",     "MINMinnesota Twins",      
 "CLECleveland Guardians",   "CHWChicago White Sox",     "KCKansas City Royals",    
 "DETDetroit Tigers",        "TEXTexas Rangers",         "LAALos Angeles Angels",   
 "HOUHouston Astros",        "SEASeattle Mariners",      "OAKOakland Athletics",    
 "ATLAtlanta Braves",       "NYMNew York Mets",         "MIAMiami Marlins",        
 "PHIPhiladelphia Phillies", "WSHWashington Nationals",  "MILMilwaukee Brewers",    
 "PITPittsburgh Pirates",    "CINCincinnati Reds",       "CHCChicago Cubs",         
 "STLSt. Louis Cardinals",   "ARIArizona Diamondbacks",  "SDSan Diego Padres",      
 "LADLos Angeles Dodgers",   "SFSan Francisco Giants",   "COLColorado Rockies"
)),

div= c(rep("AL-East",5),rep("AL-Central",5),rep("AL-West",5),
       rep("NL-East",5),rep("NL-Central",5),rep("NL-West",5))


)

standings <- merge(team_df,standings) |> arrange(div)

standings
}
