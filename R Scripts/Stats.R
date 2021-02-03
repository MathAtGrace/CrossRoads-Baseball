library(rvest)
#require(tidyverse)

load("Data/stats_by_type.Rdata")
Batting <- baseball[["Batting"]]
Pitching <- baseball[["Pitching"]]
Fielding <- baseball[["Fielding"]]
teams <- names(Batting)

#Calculate League Pitching Averages for FIP

FIP_stats <- c("ER", "HR", "SO", "HBP", "BB", "IP","ERA")
CL_Avg_P <- c()
All_stats_pitching <- c()

w = 0

for (n in seq(length(FIP_stats)-1)){
  
  for (j in teams){
    
    P <- read.csv(paste("Data/Pitching/",j,".csv",sep=""))
    
    
    for (k in seq(length(P[[FIP_stats[[n]]]]))){ 
      
      All_stats_pitching[w+k] <- P[[FIP_stats[[n]]]][k]
      #print(All_stats)
    }
    
    w  = k + w
    
  }
  CL_Avg_P[n] <- mean(All_stats_pitching)
  All_stats_pitching <- c()
  w = 0
}

CL_ERA <- (CL_Avg_P[[1]]*9)/(CL_Avg_P[[6]])
print(CL_ERA)
#append(CL_Avg_P,CL_ERA)
#print(FIP_stats)
#print(CL_Avg_P)

#get data for park effects
p_effects_urls <- paste0("http://www.dakstats.com/WebSync/Pages/Team/TeamSchedule.aspx?association=10&sg=MBA&sea=NAIMBA_2019&team=",names(teams))



#FIP

FIP_CL_constant = CL_ERA - (((13*CL_Avg_P[[2]])+(3*(CL_Avg_P[[5]]+CL_Avg_P[[4]]))-(2*CL_Avg_P[[3]]))/(CL_Avg_P[[6]]))
print(FIP_CL_constant)

pitch_stats <- function(y){
  x <- Pitching[[y]]
  FIP_team <- ((13*x$HR)+3*(x$BB+x$HBP)-(2*x$SO))/(x$IP)
  x$FIP <- FIP_team + FIP_CL_constant
  x$PR <- (x$IP*(CL_ERA - x$ERA))/9
  x
}

Pitching <- teams %>%
  lapply(pitch_stats)
names(Pitching) <- teams

#Calculate League Averages for Batting 

bat_stats <- function(y){
  x <- Batting[[y]]
  #Caught Stealing
  x$CS <- x$SBA - x$SB
  #Singles
  x$X1B <- x$H - x[,8] - x[,9] - x$HR
  #Runs Created
  x$RC <- (x$H + x$BB)*x$TB/(x$AB + x$BB)
  #OPS
  x$OPS <- x$OB + x$SLG
  #POP
  x$POP <- x$OPS + x$AVG
  #Total Average
  x$TA <- (x$TB + x$BB + x$HBP + x$SB)/(x$AB - x$H + x$SH + x$SF + x$CS + x$GDP)
  #Plate Appearances
  x$PA <- x$AB+x$BB+x$HBP+x$SF
  #Batting Average on Balls in Play
  x$BABIP <- (x$H - x$HR)/(x$AB - x$SO - x$HR + x$SF)
  #Hoban Efficiency Quotient - offense
  x$HEQO <- x$TB + x$R + x$RBI + x$SB + (0.5*x$BB)
  #Base Stealing Runs
  x$Steal_Runs <- ((x$H + x$BB - x$CS)*(x$TB + (0.7 * x$SB)))/(x$AB + x$BB + x$CS)
  x
}

Batting <- teams %>%
  lapply(bat_stats)
names(Batting) <- teams

batting_stats <- c("H","AB","BB","HBP","SF","TB", "2B","3B","HR","X1B","R")
CL_Avg_B <- c()
All_stats_batting <- c()
print(which( colnames(Batting$BC)==batting_stats[7]))
w = 0

for (n in batting_stats){
  
  for (j in teams){
    
    P <- Batting[[j]]#as.data.frame(read.csv(paste("Data/Batting/",j,".csv",sep="")))
    
    col_num =  which( colnames(P)==n )
    for (k in seq(length(P[[n]]))){ 
      
      All_stats_batting[w+k] <- P[,col_num][k]
      #print(All_stats)
    }
    
    w  = k + w
    
  }
  CL_Avg_B[n] <- sum(All_stats_batting)
  All_stats_batting <- c()
  w = 0
}

print(CL_Avg_B)

CL_OBP <- (CL_Avg_B[[1]]+CL_Avg_B[[3]]+CL_Avg_B[[4]])/(CL_Avg_B[[2]]+CL_Avg_B[[3]]+CL_Avg_B[[4]]+CL_Avg_B[[5]])
CL_SLG <- (CL_Avg_B[[6]])/(CL_Avg_B[[2]])
CL_wOBA <- ((.69)*(CL_Avg_B[["BB"]])+(.72)*(CL_Avg_B[["HBP"]])+(.89)*(CL_Avg_B[["X1B"]])+(1.27)*(CL_Avg_B[["2B"]])+(1.62)*(CL_Avg_B[["3B"]])+(2.10)*(CL_Avg_B[["HR"]]))/(CL_Avg_B[["AB"]]+CL_Avg_B[["BB"]]+CL_Avg_B[["SF"]]+CL_Avg_B[["HBP"]])
CL_PA <- CL_Avg_B[["AB"]]+CL_Avg_B[["BB"]]+CL_Avg_B[["HBP"]]+CL_Avg_B[["SF"]] 
CL_R_per_PA <- CL_Avg_B[["R"]]/CL_PA
CL_wRC <- CL_Avg_B[["R"]]
CL_wRC_per_PA <- CL_R_per_PA
  
#League adjustment for Batting Runs
ABF <- ((0.47 * CL_Avg_B[["X1B"]]) + (0.38*CL_Avg_B[["2B"]]) + (0.55*CL_Avg_B[["3B"]]) + (0.93*CL_Avg_B[["HR"]]) +(0.33*(CL_Avg_B[["BB"]]+CL_Avg_B[["HBP"]]))/(CL_Avg_B[["AB"]]-CL_Avg_B[["H"]]))
        

bat_stats2 <- function(y){
  x <- Batting[[y]]
  #OPS+
  x$OPS_plus <- (100)*(((x$OB/CL_OBP)+(x$SLG/CL_SLG))-1)
  #weighted On-Base Average
  x$wOBA <- ((.69)*(x$BB)+(.72)*(x$HBP)+(.89)*(x$X1B)+(1.27)*(x[["2B"]])+(1.62)*(x[["3B"]])+(2.10)*(x$HR))/(x$AB+x$BB+x$SF+x$HBP)
  #Batting Runs
  x$Bat_Runs <- round(((0.47*x$X1B) + (0.38*x[["2B"]]) + (0.55*x[["3B"]]) + (0.93*x$HR) + ((0.33)*x$BB + x$HBP)/(ABF *(x$AB - x$H))),digits = 5)
  #weighted Runs Above Average
  x$wRAA <- ((x$wOBA-CL_wOBA)/(1.157))*(x$PA)
  #weighted Runs Created
  x$wRC <- x$wRAA + (x$PA)*((CL_Avg_B[["R"]])/(CL_PA))
  x$wRC_plus <- (100)*((((x$wRAA/x$PA)+CL_R_per_PA)+(CL_R_per_PA-(p_factor[[y]])*(CL_R_per_PA)))/(CL_wRC_per_PA))
  x
}

Batting <- teams %>%
  lapply(bat_stats2)
names(Batting) <- teams
