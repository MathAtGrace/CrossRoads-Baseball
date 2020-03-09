Best_Players <- data.frame(wRC_plus=numeric())

#counter for teams
y = 1


for (x in Batting){
  
  wRC_plus <- c()
  wRC_name <- c()
  wRC_team <- c()
  PA <- c()
  wRC_plus <- x[["wRC_plus"]]
  wRC_name <- x[["Batting"]]
  PA <- x[["PA"]]
  team <- teams[y]
  for (z in wRC_name){
  
  wRC_team <- c(wRC_team, team[z])      
  
  
  }
  
  new.Best_Players <- data.frame(team, wRC_name,  wRC_plus,  PA)
  Best_Players <- rbind(Best_Players, new.Best_Players)
  
  y = y + 1
}

#remove NaN values
Best_Players <- Best_Players[complete.cases(Best_Players), ]

#remove Total and Opponents values
Best_Players <- Best_Players[ !(Best_Players$wRC_name %in% c("Opponents:", "Total:")), ]

Best_Players <- Best_Players[order(-Best_Players$wRC_plus),]

#require players to have at least 20 plate appearances
Best_Players <- Best_Players[!(Best_Players$PA < 20),]
