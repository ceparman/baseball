
#ML betting
library(dplyr)
library(rvest)
library(gt)

source("get_full_lines.R")
source("get_standings.R")
source("screens.R")

standings <- get_standings()

full_line <- get_full_lines(standings)


BBAW <- BBAW(full_line) |>  select(team,BBAW)


TWTM <- TWTM(full_line) |>  select(team,TWTM)

DD <- DD(full_line) |>  select(team,DD)

PMU <- PMU(full_line) |>  select(team,PMU)


ML_matrix <- full_line |> select(team,ML) |> 
  full_join(BBAW) |> full_join(TWTM) |> full_join(DD) |> full_join(PMU) |>
 rowwise() %>% 
  mutate(`Bet Weight` = sum(across( c(BBAW,TWTM,DD,PMU)  ),na.rm=T )) |>
  filter(`Bet Weight` >0 ) |>
  rename(Team = team) |>
  relocate(ML, .after = last_col()-1) |>
  arrange(desc(`Bet Weight`))


ML_matrix |>
  gt() |>
  sub_missing(missing_text = "") |> 
  tab_style(
    style = cell_borders(
      sides = c("left"),
      weight = px(2)),
    locations = cells_body(
      columns = c(BBAW,ML )
    )
  ) |>
  
  tab_style(
    style = cell_borders(
      sides = c("left"),
      weight = px(1),
    color = "#cccccc"),
    locations = cells_body(
      columns = c(TWTM,DD,PMU,`Bet Weight` )
      
    )
  ) |>
  tab_header(
    title = paste("Baseball Moneyline bets",Sys.Date())
  ) |>
  
  tab_footnote(
    footnote = "BBAW - Back Bad Teams after a Win") |>
 
   tab_footnote(
  footnote = "TWTM - Winning Teams Meet, Take the Under") |>
  
  tab_footnote(
    footnote = "DD - Focus on Divisional Dogs") |>
  tab_footnote(
    footnote = "PMU - Take Advantage of Plus-Money Underdogs") 
                       
  

