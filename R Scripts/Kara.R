library(rvest)
#require(tidyverse)

load("Data/stats_by_type.Rdata")
Batting <- baseball[["Batting"]]
Pitching <- baseball[["Pitching"]]
Fielding <- baseball[["Fielding"]]
teams <- names(batting)


#Calculate League Pitching Averages for FIP

FIP_stats <- c("ER", "HR", "SO", "HBP", "BB", "IP","ERA")
CL_Avg_P <- c()
All_stats <- c()

w = 0

for (n in seq(length(FIP_stats)-1)){
  
  for (j in teams){
    
    P <- read.csv(paste("Data/Pitching/",j,".csv",sep=""))
    
    
    for (k in seq(length(P[[FIP_stats[[n]]]]))){ 
      
      All_stats[w+k] <- P[[FIP_stats[[n]]]][k]
      #print(All_stats)
    }
    
    w  = k + w
    
  }
  CL_Avg_P[n] <- mean(All_stats)
  All_stats <- c()
  w = 0
}

CL_ERA <- (CL_Avg_P[[1]]*9)/(CL_Avg_P[[6]])
print(CL_ERA)
#append(CL_Avg_P,CL_ERA)
#print(FIP_stats)
#print(CL_Avg_P)

#get data for park effects
p_effects <- read_html("http://www.dakstats.com/WebSync/Pages/Team/TeamSchedule.aspx?association=10&sg=MBA&sea=NAIMBA_2019&team=1679")

tbls_effects <- p_effects %>%
  html_nodes("table") %>%
  .[36] %>%
  html_table(fill = TRUE)

Team_Schedule <- data.frame(tbls_effects[[1]])
Team_Schedule <- Team_Schedule[!apply(is.na(Team_Schedule) | Team_Schedule == "", 1, all),]

#FIP

FIP_CL_constant = CL_ERA - (((13*CL_Avg_P[[2]])+(3*(CL_Avg_P[[5]]+CL_Avg_P[[4]]))-(2*CL_Avg_P[[3]]))/(CL_Avg_P[[6]]))
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

#Calculate League Batting Averages for 

batting_stats <- c("ERA", "HR", "SO", "HBP", "BB", "IP")
CL_Avg_B <- c()
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
