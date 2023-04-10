

get_full_lines <- function(standings){
  
  
  
  tab <- rvest::read_html("https://www.espn.com/mlb/lines") |> rvest::html_table()
  
  
  
  matchline <- function(line,standings){
    
    
    which(unlist(purrr::map(standings[,1],stringr::str_detect,as.character(line))))
    
    
  }
  
  
  away_line <-unlist(lapply(tab, function(x) {matchline(x[1,1],standings)}))
  
  home_line <-unlist(lapply(tab, function(x) {matchline(x[2,1],standings)}))
  
  full_table <- data.frame() 
  
  for( i in 1:length(tab)){
    
    at <-  cbind ( tab[[i]][1,-3],standings[away_line[i],])
    
    names(at)[1] <- "team"
    
   # names(at) <- paste0(names(at),"-away")
    
    at$`H/A` <- "Away"
    
    at$game <- i
    
    full_table <-  rbind(full_table, at)
    
    ht <-  cbind ( tab[[i]][2,-3],standings[home_line[i],])
    
    names(ht)[1] <- "team"
    
    ht$`H/A` <- "Home"
    
    ht$game <- i
    
   # names(ht) <- paste0(names(ht),"-home")
    full_table <-  rbind(full_table, ht)
    
    
    
  }
  
  full_table <-  full_table[,-6]
  
  
  
  full_table <- full_table |> mutate( GB = if_else( GB == "-","0",GB )) |>
    mutate(across(c(PCT,GB,W,L,RS,RA,DIFF) ,as.numeric))
  
  
  
}
