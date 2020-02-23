#place to store each team's elo rating
ratings <- data.frame(team = teams)
ratings <- ratings %>%
  mutate(elo = 1500)
View(ratings)

#not working how I intend it to
add_elo_score <- function(x){
  schedules[[x]] <- schedules[[x]] %>%
    mutate(result = ifelse(Team_Score > Opp_Score, 1, 0))
}

schedules <- lapply(teams, add_elo_score)
names(schedules) <- teams

#matches <- matches %>%
  #mutate(result = if_else(home_score > away_score, 1, 0))