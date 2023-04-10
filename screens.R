#Back Bad Teams after a Win

BBAW <- function(full_lines){
  
  
    full_lines |> filter(STRK == 'W1' & PCT < .4) |> mutate( BBAW = 1)
  
  
  
}


#When Two Winning Teams Meet, Take the Under

TWTM <- function(full_line) {
  
  
  
games <-   full_line |> filter(PCT > .55) |> group_by(game) |> 
                 summarise(n=n()) |> filter(n ==2) |> pull(game)
   
  
full_line |> filter(game %in% games) |> filter(ML > 0)   |> mutate( TWTM = 1)
  
}



#Focus on Divisional Dogs

DD <- function(full_line) {
  
  
 full_line |> group_by(game) |> mutate( num = n_distinct(div)) |>
   filter(num == 1) |> filter(ML > 0) |> ungroup() |> mutate( DD = round(pmin(2.5, (ML-100)/40),1))
  
  
  
}

# Take Advantage of Plus-Money Underdogs

PMU <- function(full_line) {
  
  
  full_line |> filter(ML >100) |> mutate( PMU = round( pmin(2.5, (ML-100)/40),1))
  

}
