library(rvest)
#require(tidyverse)

load("Data/stats_by_type.Rdata")
Batting <- baseball[["Batting"]]
Pitching <- baseball[["Pitching"]]
Fielding <- baseball[["Fielding"]]
teams <- names(batting)


#Calculate League Average for FIP

FIP_stats <- c("ERA", "HR", "SO", "HBP", "BB", "IP")
CL_Avg_P <- c()
All_stats <- c()

w = 0

for (n in FIP_stats){
  
  for (j in teams){
    
    P <- read.csv(paste("Data/Pitching/",j,".csv",sep=""))
    
    
    for (k in seq(length(P[[n]]))){ 
      
      All_stats[w+k] <- P[[n]][k]
      #print(All_stats)
    }
    
    w  = k + w
    
  }
  CL_Avg_P[n] <- mean(All_stats)
  All_stats <- c()
  w = 0
}

print(CL_Avg_P)

#get data for park effects
p_effects <- read_html("http://www.dakstats.com/WebSync/Pages/Team/TeamSchedule.aspx?association=10&sg=MBA&sea=NAIMBA_2019&team=1679")

tbls_effects <- p_effects %>%
  html_nodes("table") %>%
  .[36] %>%
  html_table(fill = TRUE)

Team_Schedule <- data.frame(tbls_effects[[1]])
Team_Schedule <- Team_Schedule[!apply(is.na(Team_Schedule) | Team_Schedule == "", 1, all),]

#FIP

FIP_CL_constant = CL_Avg_P[["ERA"]] - (((13*CL_Avg_P[["HR"]])+(3*(CL_Avg_P[["BB"]]+CL_Avg_P[["HBP"]]))-(2*CL_Avg_P[["SO"]]))/(CL_Avg_P[["IP"]]))
print(FIP_CL_constant)

pitch_stats <- function(y){
  x <- Pitching[[y]]
  FIP_team <- ((13*x$HR)+3*(x$BB+x$HBP)-(2*x$SO))/(x$IP)
  x$FIP <- FIP_team + FIP_CL_constant
  x
}

Pitching <- teams %>%
  lapply(pitch_stats)
names(Pitching) <- teams
