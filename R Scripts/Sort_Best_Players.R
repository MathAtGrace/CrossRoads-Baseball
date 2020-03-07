Best_Players <- data.frame(wRC_plus=numeric())

#counter for teams
y = 1


for (x in Batting){
  
  wRC_temp_vec <- c()
  wRC_name <- c()
  wRC_team <- c()
  wRC_temp_vec <- x[["wRC_plus"]]
  wRC_name <- x[["Batting"]]
  team <- teams[y]
  for (z in wRC_name){
  
  wRC_team <- c(wRC_team, team[z])      
  
  
  }
  
  new.Best_Players <- data.frame(wRC_name, wRC_temp_vec, team)
  Best_Players <- rbind(Best_Players, new.Best_Players)
  
  y = y + 1
}