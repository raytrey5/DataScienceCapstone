library(readr)
nba_stats <- read_csv("Downloads/nba.games.stats.csv")
str(nba_stats)
nba_stats <- as.data.frame(nba_stats)
#Get rid of first column
nba_stats$X1 <- NULL
summary(nba_stats)
str(nba_stats)
# Convert characters into factors
nba_stats$Team <- as.factor(nba_stats$Team)
nba_stats$Home <- as.factor(nba_stats$Home)
nba_stats$Opponent <- as.factor(nba_stats$Opponent)
nba_stats$WINorLOSS <- as.factor(nba_stats$WINorLOSS)
str(nba_stats)
# Change Column names
names(nba_stats)[names(nba_stats) == "Home"] <- "HomeOrAway"
names(nba_stats)[names(nba_stats) == "FieldGoals."] <- "FieldGoal%"
str(nba_stats)
names(nba_stats)[names(nba_stats) == "FieldGoals"] <- "FieldGoalsMade"
names(nba_stats)[names(nba_stats) == "X3PointShots"] <- "3PointShotsMade"
names(nba_stats)[names(nba_stats) == "X3PointShotsAttempted"] <- "3PointShotsAttemted"
names(nba_stats)[names(nba_stats) == "X3PointShots."] <- "3Point%"
names(nba_stats)[names(nba_stats) == "FreeThrows"] <- "FreeThrowsMade"
names(nba_stats)[names(nba_stats) == "FreeThrows."] <- "FreeThrow%"
names(nba_stats)[names(nba_stats) == "Opp.FieldGoals"] <- "OpponentFieldGoalsMade"
names(nba_stats)[names(nba_stats) == "Opp.FieldGoalsAttempted"] <- "OpponentFieldGoalsAttempted"
names(nba_stats)[names(nba_stats) == "Opp.FieldGoals."] <- "OpponentFieldGoal%"
names(nba_stats)[names(nba_stats) == "Opp.3PointShots"] <- "Opponent3PointShotsMade"
names(nba_stats)[names(nba_stats) == "Opp.3PointShotsAttempted"] <- "Opponent3PointShotsAttempted"
names(nba_stats)[names(nba_stats) == "Opp.3PointShots."] <- "Opponent3Point%"
names(nba_stats)[names(nba_stats) == "Opp.FreeThrows"] <- "OpponentFreeThrowsMade"
names(nba_stats)[names(nba_stats) == "Opp.FreeThrowsAttempted"] <- "OpponentFreeThrowsAttempted"
names(nba_stats)[names(nba_stats) == "Opp.FreeThrows."] <- "OpponentFreeThrow%"
names(nba_stats)[names(nba_stats) == "Opp.OffRebounds"] <- "OpponentOffRebounds"
names(nba_stats)[names(nba_stats) == "Opp.TotalRebounds"] <- "OpponentTotalRebounds"
names(nba_stats)[names(nba_stats) == "Opp.Assists"] <- "OpponentAssists"
names(nba_stats)[names(nba_stats) == "Opp.Steals"] <- "OpponentSteals"
names(nba_stats)[names(nba_stats) == "Opp.Blocks"] <- "OpponentBlocks"
names(nba_stats)[names(nba_stats) == "Opp.Turnovers"] <- "OpponentTurnovers"
names(nba_stats)[names(nba_stats) == "Opp.TotalFouls"] <- "OpponentTotalFouls"

#Check structure of dataframe
str(nba_stats)

# Check for missing values
sum(is.na(nba_stats))

View(nba_stats)
#  Split dataframe into 4 seasons by date
nba20142015season <- nba_stats[nba_stats$Date >= "2014-10-28" & nba_stats$Date <= "2015-04-15",]
nba20152016season <- nba_stats[nba_stats$Date >= "2015-10-27" & nba_stats$Date <= "2016-04-13",]
nba20162017season <- nba_stats[nba_stats$Date >= "2016-10-25" & nba_stats$Date <= "2017-04-12",]
nba20172018season <- nba_stats[nba_stats$Date >= "2017-10-17" & nba_stats$Date <= "2018-04-11",]


# Add assist to turnover ratio for each season and Overall
nba20142015season["Assist/Turnover Ratio"] <- nba20142015season$Assists/nba20142015season$Turnovers
nba20152016season["Assist/Turnover Ratio"] <- nba20152016season$Assists/nba20152016season$Turnovers
nba20162017season["Assist/Turnover Ratio"] <- nba20162017season$Assists/nba20162017season$Turnovers
nba20172018season["Assist/Turnover Ratio"] <- nba20172018season$Assists/nba20172018season$Turnovers
nba_stats["Assist/Turnover Ratio"] <- nba_stats$Assists/nba_stats$Turnovers


# Calculate Wins for Eastern Conference Teams each season
ATLwins20142015 <- nba20142015season$Team == "ATL" & nba20142015season$WINorLOSS == "W"
sum(ATLwins20142015)
BOSwins20142015 <- nba20142015season$Team == "BOS" & nba20142015season$WINorLOSS == "W"
sum(BOSwins20142015)
BRKwins20142015 <- nba20142015season$Team == "BRK" & nba20142015season$WINorLOSS == "W"
sum(BRKwins20142015)
CHOwins20142015 <- nba20142015season$Team == "CHO" & nba20142015season$WINorLOSS == "W"
sum(CHOwins20142015)
CHIwins20142015 <- nba20142015season$Team == "CHI" & nba20142015season$WINorLOSS == "W"
sum(CHIwins20142015)
CLEwins20142015 <- nba20142015season$Team == "CLE" & nba20142015season$WINorLOSS == "W"
sum(CLEwins20142015)
DETwins20142015 <- nba20142015season$Team == "DET" & nba20142015season$WINorLOSS == "W"
sum(DETwins20142015)
INDwins20142015 <- nba20142015season$Team == "IND" & nba20142015season$WINorLOSS == "W"
sum(INDwins20142015)
MIAwins20142015 <- nba20142015season$Team == "MIA" & nba20142015season$WINorLOSS == "W"
sum(MIAwins20142015)
MILwins20142015 <- nba20142015season$Team == "MIL" & nba20142015season$WINorLOSS == "W"
sum(MILwins20142015)
NYKwins20142015 <- nba20142015season$Team == "NYK" & nba20142015season$WINorLOSS == "W"
sum(NYKwins20142015)
ORLwins20142015 <- nba20142015season$Team == "ORL" & nba20142015season$WINorLOSS == "W"
sum(ORLwins20142015)
PHIwins20142015 <- nba20142015season$Team == "PHI" & nba20142015season$WINorLOSS == "W"
sum(PHIwins20142015)
TORwins20142015 <- nba20142015season$Team == "TOR" & nba20142015season$WINorLOSS == "W"
sum(TORwins20142015)
WASwins20142015 <- nba20142015season$Team == "WAS" & nba20142015season$WINorLOSS == "W"
sum(WASwins20142015)

# Win's vector for Eastern Conference
EastWins20142015 <- c(sum(ATLwins20142015),sum(BOSwins20142015),sum(BRKwins20142015),sum(CHIwins20142015),sum(CHOwins20142015),sum(CLEwins20142015),sum(DETwins20142015),sum(INDwins20142015),sum(MIAwins20142015),sum(MILwins20142015),sum(NYKwins20142015),sum(ORLwins20142015),sum(PHIwins20142015),sum(TORwins20142015),sum(WASwins20142015))
EastWins20142015

View(nba20142015season)




# Atlanta 2014 2015
ATL20142015 <- nba20142015season[1:82,]

# Boston 2014 2015
BOS20142015 <- nba20142015season[83:164,]

#Brooklyn 2014 2015
BRK20142015 <- nba20142015season[165:246,]

# CHO 2014 2015
CHO20142015 <- nba20142015season[247:328,]
#  Chicago 2014 2015
CHI20142015 <- nba20142015season[329:410,]
# Cleveland 2014 2015
CLE20142015 <- nba20142015season[411:492,]
# DET 2014 2015
DET20142015 <- nba20142015season[657:738,]
# IND 2014 2015
IND20142015 <- nba20142015season[903:984,]
# MIA 2014 2015
MIA20142015 <- nba20142015season[1231:1312,]
# MIL 2014 2015 
MIL20142015 <- nba20142015season[1313:1394,]
# NYK 2014 2015
NYK20142015 <- nba20142015season[1559:1640,]
# ORL 2014 2015
ORL20142015 <- nba20142015season[1723:1804,]
# PHI 2014 2015
PHI20142015 <- nba20142015season[1805:1886,]
# TOR 2014 2015
TOR20142015 <- nba20142015season[2215:2296,]
# WAS 2014 2015
WAS20142015 <- nba20142015season[2379:2460,]

EastThreePointPercentage20142015 <- c(mean(ATL20142015$`3Point%`), mean(BOS20142015$`3Point%`), mean(BRK20142015$`3Point%`),mean(CHI20142015$`3Point%`),mean(CHO20142015$`3Point%`),mean(CLE20142015$`3Point%`),mean(DET20142015$`3Point%`),mean(IND20142015$`3Point%`),mean(MIA20142015$`3Point%`),mean(MIL20142015$`3Point%`),mean(NYK20142015$`3Point%`),mean(ORL20142015$`3Point%`),mean(PHI20142015$`3Point%`),mean(TOR20142015$`3Point%`),mean(WAS20142015$`3Point%`))    
EastTeams <- c("ATL","BOS","BRK","CHI","CHO","CLE","DET","IND","MIA","MIL","NYK","ORL","PHI","TOR","WAS")
EasternConference20142015TeamsDf <- data.frame(EastTeams, EastWins20142015, EastThreePointPercentage20142015)
  
EasternConference20142015TeamsDf$EastWins20142015 <- as.numeric(EasternConference20142015TeamsDf$EastWins20142015)
ggplot(EasternConference20142015TeamsDf, aes(x = EastThreePointPercentage20142015, y = EastWins20142015, colour = EastTeams)) + geom_point() + ggtitle("2014-2015 Eastern Conference Teams: 3point% vs Wins")


EastThreePointPercentage20142015 <- c(mean(ATL20142015$`3Point%`), mean(BOS20142015$`3Point%`), mean(BRK20142015$`3Point%`),mean(CHI20142015$`3Point%`),mean(CHO20142015$`3Point%`),mean(CLE20142015$`3Point%`),mean(DET20142015$`3Point%`),mean(IND20142015$`3Point%`),mean(MIA20142015$`3Point%`),mean(MIL20142015$`3Point%`),mean(NYK20142015$`3Point%`),mean(ORL20142015$`3Point%`),mean(PHI20142015$`3Point%`),mean(TOR20142015$`3Point%`),mean(WAS20142015$`3Point%`))    
EastThreePointAttempts20142015 <- c(mean(ATL20142015$`3PointShotsAttemted`), mean(BOS20142015$`3PointShotsAttemted`), mean(BRK20142015$`3PointShotsAttemted`),mean(CHI20142015$`3PointShotsAttemted`),mean(CHO20142015$`3PointShotsAttemted`),mean(CLE20142015$`3PointShotsAttemted`),mean(DET20142015$`3PointShotsAttemted`),mean(IND20142015$`3PointShotsAttemted`),mean(MIA20142015$`3PointShotsAttemted`),mean(MIL20142015$`3PointShotsAttemted`),mean(NYK20142015$`3PointShotsAttemted`),mean(ORL20142015$`3PointShotsAttemted`),mean(PHI20142015$`3PointShotsAttemted`),mean(TOR20142015$`3PointShotsAttemted`),mean(WAS20142015$`3PointShotsAttemted`))  
EastThreePointMakes20142015 <- c(mean(ATL20142015$`3PointShotsMade`), mean(BOS20142015$`3PointShotsMade`), mean(BRK20142015$`3PointShotsMade`),mean(CHI20142015$`3PointShotsMade`),mean(CHO20142015$`3PointShotsMade`),mean(CLE20142015$`3PointShotsMade`),mean(DET20142015$`3PointShotsMade`),mean(IND20142015$`3PointShotsMade`),mean(MIA20142015$`3PointShotsMade`),mean(MIL20142015$`3PointShotsMade`),mean(NYK20142015$`3PointShotsMade`),mean(ORL20142015$`3PointShotsMade`),mean(PHI20142015$`3PointShotsMade`),mean(TOR20142015$`3PointShotsMade`),mean(WAS20142015$`3PointShotsMade`))  
EastOpponent3PointPercentage20142015 <- c(mean(ATL20142015$`Opponent3Point%`), mean(BOS20142015$`Opponent3Point%`), mean(BRK20142015$`Opponent3Point%`),mean(CHI20142015$`Opponent3Point%`),mean(CHO20142015$`Opponent3Point%`),mean(CLE20142015$`Opponent3Point%`),mean(DET20142015$`Opponent3Point%`),mean(IND20142015$`Opponent3Point%`),mean(MIA20142015$`Opponent3Point%`),mean(MIL20142015$`Opponent3Point%`),mean(NYK20142015$`Opponent3Point%`),mean(ORL20142015$`Opponent3Point%`),mean(PHI20142015$`Opponent3Point%`),mean(TOR20142015$`Opponent3Point%`),mean(WAS20142015$`Opponent3Point%`))  


EastFieldGoalPercentage20142015 <- c(mean(ATL20142015$`FieldGoal%`), mean(BOS20142015$`FieldGoal%`), mean(BRK20142015$`FieldGoal%`),mean(CHI20142015$`FieldGoal%`),mean(CHO20142015$`FieldGoal%`),mean(CLE20142015$`FieldGoal%`),mean(DET20142015$`FieldGoal%`),mean(IND20142015$`FieldGoal%`),mean(MIA20142015$`FieldGoal%`),mean(MIL20142015$`FieldGoal%`),mean(NYK20142015$`FieldGoal%`),mean(ORL20142015$`FieldGoal%`),mean(PHI20142015$`FieldGoal%`),mean(TOR20142015$`FieldGoal%`),mean(WAS20142015$`FieldGoal%`))  

EastOpponentFGpercentage20142015 <- c(mean(ATL20142015$`OpponentFieldGoal%`), mean(BOS20142015$`OpponentFieldGoal%`), mean(BRK20142015$`OpponentFieldGoal%`),mean(CHI20142015$`OpponentFieldGoal%`),mean(CHO20142015$`OpponentFieldGoal%`),mean(CLE20142015$`OpponentFieldGoal%`),mean(DET20142015$`OpponentFieldGoal%`),mean(IND20142015$`OpponentFieldGoal%`),mean(MIA20142015$`OpponentFieldGoal%`),mean(MIL20142015$`OpponentFieldGoal%`),mean(NYK20142015$`OpponentFieldGoal%`),mean(ORL20142015$`OpponentFieldGoal%`),mean(PHI20142015$`OpponentFieldGoal%`),mean(TOR20142015$`OpponentFieldGoal%`),mean(WAS20142015$`OpponentFieldGoal%`))  

EastAssistToTurnoverRatio20142015 <- c(mean(ATL20142015$`Assist/Turnover Ratio`), mean(BOS20142015$`Assist/Turnover Ratio`), mean(BRK20142015$`Assist/Turnover Ratio`),mean(CHI20142015$`Assist/Turnover Ratio`),mean(CHO20142015$`Assist/Turnover Ratio`),mean(CLE20142015$`Assist/Turnover Ratio`),mean(DET20142015$`Assist/Turnover Ratio`),mean(IND20142015$`Assist/Turnover Ratio`),mean(MIA20142015$`Assist/Turnover Ratio`),mean(MIL20142015$`Assist/Turnover Ratio`),mean(NYK20142015$`Assist/Turnover Ratio`),mean(ORL20142015$`Assist/Turnover Ratio`),mean(PHI20142015$`Assist/Turnover Ratio`),mean(TOR20142015$`Assist/Turnover Ratio`),mean(WAS20142015$`Assist/Turnover Ratio`))  

EastFreeThrowPercentage20142015 <- c(mean(ATL20142015$`FreeThrow%`), mean(BOS20142015$`FreeThrow%`), mean(BRK20142015$`FreeThrow%`),mean(CHI20142015$`FreeThrow%`),mean(CHO20142015$`FreeThrow%`),mean(CLE20142015$`FreeThrow%`),mean(DET20142015$`FreeThrow%`),mean(IND20142015$`FreeThrow%`),mean(MIA20142015$`FreeThrow%`),mean(MIL20142015$`FreeThrow%`),mean(NYK20142015$`FreeThrow%`),mean(ORL20142015$`FreeThrow%`),mean(PHI20142015$`FreeThrow%`),mean(TOR20142015$`FreeThrow%`),mean(WAS20142015$`FreeThrow%`))  

EastOpponentFreeThrowPercentage20142015 <- c(mean(ATL20142015$`OpponentFreeThrow%`), mean(BOS20142015$`OpponentFreeThrow%`), mean(BRK20142015$`OpponentFreeThrow%`),mean(CHI20142015$`OpponentFreeThrow%`),mean(CHO20142015$`OpponentFreeThrow%`),mean(CLE20142015$`OpponentFreeThrow%`),mean(DET20142015$`OpponentFreeThrow%`),mean(IND20142015$`OpponentFreeThrow%`),mean(MIA20142015$`OpponentFreeThrow%`),mean(MIL20142015$`OpponentFreeThrow%`),mean(NYK20142015$`OpponentFreeThrow%`),mean(ORL20142015$`OpponentFreeThrow%`),mean(PHI20142015$`OpponentFreeThrow%`),mean(TOR20142015$`OpponentFreeThrow%`),mean(WAS20142015$`OpponentFreeThrow%`))  
Championship <- c("No","No","No","No","No","No","No","No","No","No","No","No","No","No","No")
EasternConference20142015TeamsDf <- data.frame(EastTeams, EastWins20142015, EastThreePointMakes20142015,EastThreePointAttempts20142015, EastThreePointPercentage20142015, EastOpponent3PointPercentage20142015, EastFieldGoalPercentage20142015, EastOpponentFGpercentage20142015, EastFreeThrowPercentage20142015, EastOpponentFreeThrowPercentage20142015, EastAssistToTurnoverRatio20142015, Championship)

g1E2015 <- ggplot(EasternConference20142015TeamsDf, aes (x = EastThreePointMakes20142015, y = EastWins20142015, colour = EastTeams)) + geom_point() + ggtitle("2014-2015 Eastern Conference Teams: 3point Makes vs Wins")
g2E2015 <- ggplot(EasternConference20142015TeamsDf, aes (x = EastThreePointAttempts20142015, y = EastWins20142015, colour = EastTeams)) + geom_point() + ggtitle("2014-2015 Eastern Conference Teams: 3pointAttempts vs Wins")

g3E2015 <- ggplot(EasternConference20142015TeamsDf, aes (x = EastAssists20142015, y = EastWins20142015, colour = EastTeams)) + geom_point() + ggtitle("2014-2015 Eastern Conference Teams: Assists vs Wins")



# Calculate Wins for Eastern Conference Teams each season
DALwins20142015 <- nba20142015season$Team == "DAL" & nba20142015season$WINorLOSS == "W"
sum(DALwins20142015)
DENwins20142015 <- nba20142015season$Team == "DEN" & nba20142015season$WINorLOSS == "W"
sum(DENwins20142015)
GSWwins20142015 <- nba20142015season$Team == "GSW" & nba20142015season$WINorLOSS == "W"
sum(GSWwins20142015)
HOUwins20142015 <- nba20142015season$Team == "HOU" & nba20142015season$WINorLOSS == "W"
sum(HOUwins20142015)
LACwins20142015 <- nba20142015season$Team == "LAC" & nba20142015season$WINorLOSS == "W"
sum(LACwins20142015)
LALwins20142015 <- nba20142015season$Team == "LAL" & nba20142015season$WINorLOSS == "W"
sum(LALwins20142015)
MEMwins20142015 <- nba20142015season$Team == "MEM" & nba20142015season$WINorLOSS == "W"
sum(MEMwins20142015)
MINwins20142015 <- nba20142015season$Team == "MIN" & nba20142015season$WINorLOSS == "W"
sum(MINwins20142015)
NOPwins20142015 <- nba20142015season$Team == "NOP" & nba20142015season$WINorLOSS == "W"
sum(NOPwins20142015)
OKCwins20142015 <- nba20142015season$Team == "OKC" & nba20142015season$WINorLOSS == "W"
sum(OKCwins20142015)
PHOwins20142015 <- nba20142015season$Team == "PHO" & nba20142015season$WINorLOSS == "W"
sum(PHOwins20142015)
PORwins20142015 <- nba20142015season$Team == "POR" & nba20142015season$WINorLOSS == "W"
sum(PORwins20142015)
SACwins20142015 <- nba20142015season$Team == "SAC" & nba20142015season$WINorLOSS == "W"
sum(SACwins20142015)
SASwins20142015 <- nba20142015season$Team == "SAS" & nba20142015season$WINorLOSS == "W"
sum(SASwins20142015)
UTAwins20142015 <- nba20142015season$Team == "UTA" & nba20142015season$WINorLOSS == "W"
sum(UTAwins20142015)

# Win's Vector for Western Conference
WestWins20142015 <- c(sum(DALwins20142015),sum(DENwins20142015),sum(GSWwins20142015),sum(HOUwins20142015),sum(LACwins20142015),sum(LALwins20142015),sum(MEMwins20142015),sum(MINwins20142015),sum(NOPwins20142015),sum(OKCwins20142015),sum(PHOwins20142015),sum(PORwins20142015),sum(SACwins20142015),sum(SASwins20142015),sum(UTAwins20142015))

# Dallas 2014 2015
DAL20142015 <- nba20142015season[493:574,]
# Denver 2014 2015
DEN20142015 <- nba20142015season[575:656,]
#Golden State 2014 2015
GSW20142015 <- nba20142015season[739:820,]
# Houston 2014 2015
HOU20142015 <- nba20142015season[821:902,]
# LA Clippers 2014 2015
LAC20142015 <- nba20142015season[985:1066,]
# LA Lakers 2014 2015
LAL20142015 <- nba20142015season[1067:1148,]
# Memphis 2014 2015
MEM20142015 <- nba20142015season[1149:1230,]
# Minnesota 2014 2015
MIN20142015 <- nba20142015season[1395:1476,]
# New Orleans 2014 2015 
NOP20142015 <- nba20142015season[1477:1558,]
# OKC 2014 2015
OKC20142015 <- nba20142015season[1641:1722,]
# PHO 2014 2015
PHO20142015 <- nba20142015season[1887:1968,]
# Portland 2014 2015
POR20142015 <- nba20142015season[1969:2050,]
# Sacramento 2014 2015
SAC20142015 <- nba20142015season[2051:2132,]
# San Antonio 2014 2015
SAS20142015 <- nba20142015season[2133:2214,]
# Utah 2014 2015
UTA20142015 <- nba20142015season[2297:2378,]

WestTeams <- c("DAL","DEN","GSW","HOU","LAC","LAL","MEM","MIN","NOP","OKC","PHO","POR","SAC","SAS","UTA")
WestThreePointPercentage20142015 <- c(mean(DAL20142015$`3Point%`), mean(DEN20142015$`3Point%`), mean(GSW20142015$`3Point%`),mean(HOU20142015$`3Point%`),mean(LAC20142015$`3Point%`),mean(LAL20142015$`3Point%`),mean(MEM20142015$`3Point%`),mean(MIN20142015$`3Point%`),mean(NOP20142015$`3Point%`),mean(OKC20142015$`3Point%`),mean(PHO20142015$`3Point%`),mean(POR20142015$`3Point%`),mean(SAC20142015$`3Point%`),mean(SAS20142015$`3Point%`),mean(UTA20142015$`3Point%`))

WestThreePointAttempts20142015 <- c(mean(DAL20142015$`3PointShotsAttemted`), mean(DEN20142015$`3PointShotsAttemted`), mean(GSW20142015$`3PointShotsAttemted`),mean(HOU20142015$`3PointShotsAttemted`),mean(LAC20142015$`3PointShotsAttemted`),mean(LAL20142015$`3PointShotsAttemted`),mean(MEM20142015$`3PointShotsAttemted`),mean(MIN20142015$`3PointShotsAttemted`),mean(NOP20142015$`3PointShotsAttemted`),mean(OKC20142015$`3PointShotsAttemted`),mean(PHO20142015$`3PointShotsAttemted`),mean(POR20142015$`3PointShotsAttemted`),mean(SAC20142015$`3PointShotsAttemted`),mean(SAS20142015$`3PointShotsAttemted`),mean(UTA20142015$`3PointShotsAttemted`))

WestThreePointMakes20142015 <- c(mean(DAL20142015$`3PointShotsMade`), mean(DEN20142015$`3PointShotsMade`), mean(GSW20142015$`3PointShotsMade`),mean(HOU20142015$`3PointShotsMade`),mean(LAC20142015$`3PointShotsMade`),mean(LAL20142015$`3PointShotsMade`),mean(MEM20142015$`3PointShotsMade`),mean(MIN20142015$`3PointShotsMade`),mean(NOP20142015$`3PointShotsMade`),mean(OKC20142015$`3PointShotsMade`),mean(PHO20142015$`3PointShotsMade`),mean(POR20142015$`3PointShotsMade`),mean(SAC20142015$`3PointShotsMade`),mean(SAS20142015$`3PointShotsMade`),mean(UTA20142015$`3PointShotsMade`))  
WestOpponent3PointPercentage20142015 <- c(mean(DAL20142015$`Opponent3Point%`), mean(DEN20142015$`Opponent3Point%`), mean(GSW20142015$`Opponent3Point%`),mean(HOU20142015$`Opponent3Point%`),mean(LAC20142015$`Opponent3Point%`),mean(LAL20142015$`Opponent3Point%`),mean(MEM20142015$`Opponent3Point%`),mean(MIN20142015$`Opponent3Point%`),mean(NOP20142015$`Opponent3Point%`),mean(OKC20142015$`Opponent3Point%`),mean(PHO20142015$`Opponent3Point%`),mean(POR20142015$`Opponent3Point%`),mean(SAC20142015$`Opponent3Point%`),mean(SAS20142015$`Opponent3Point%`),mean(UTA20142015$`Opponent3Point%`))  

WestFieldGoalPercentage20142015 <- c(mean(DAL20142015$`FieldGoal%`), mean(DEN20142015$`FieldGoal%`), mean(GSW20142015$`FieldGoal%`),mean(HOU20142015$`FieldGoal%`),mean(LAC20142015$`FieldGoal%`),mean(LAL20142015$`FieldGoal%`),mean(MEM20142015$`FieldGoal%`),mean(MIN20142015$`FieldGoal%`),mean(NOP20142015$`FieldGoal%`),mean(OKC20142015$`FieldGoal%`),mean(PHO20142015$`FieldGoal%`),mean(POR20142015$`FieldGoal%`),mean(SAC20142015$`FieldGoal%`),mean(SAS20142015$`FieldGoal%`),mean(UTA20142015$`FieldGoal%`))  

WestOpponentFGpercentage20142015 <- c(mean(DAL20142015$`OpponentFieldGoal%`), mean(DEN20142015$`OpponentFieldGoal%`), mean(GSW20142015$`OpponentFieldGoal%`),mean(HOU20142015$`OpponentFieldGoal%`),mean(LAC20142015$`OpponentFieldGoal%`),mean(LAL20142015$`OpponentFieldGoal%`),mean(MEM20142015$`OpponentFieldGoal%`),mean(MIN20142015$`OpponentFieldGoal%`),mean(NOP20142015$`OpponentFieldGoal%`),mean(OKC20142015$`OpponentFieldGoal%`),mean(PHO20142015$`OpponentFieldGoal%`),mean(POR20142015$`OpponentFieldGoal%`),mean(SAC20142015$`OpponentFieldGoal%`),mean(SAS20142015$`OpponentFieldGoal%`),mean(UTA20142015$`OpponentFieldGoal%`))  

WestAssistToTurnoverRatio20142015 <- c(mean(DAL20142015$`Assist/Turnover Ratio`), mean(DEN20142015$`Assist/Turnover Ratio`), mean(GSW20142015$`Assist/Turnover Ratio`),mean(HOU20142015$`Assist/Turnover Ratio`),mean(LAC20142015$`Assist/Turnover Ratio`),mean(LAL20142015$`Assist/Turnover Ratio`),mean(MEM20142015$`Assist/Turnover Ratio`),mean(MIN20142015$`Assist/Turnover Ratio`),mean(NOP20142015$`Assist/Turnover Ratio`),mean(OKC20142015$`Assist/Turnover Ratio`),mean(PHO20142015$`Assist/Turnover Ratio`),mean(POR20142015$`Assist/Turnover Ratio`),mean(SAC20142015$`Assist/Turnover Ratio`),mean(SAS20142015$`Assist/Turnover Ratio`),mean(UTA20142015$`Assist/Turnover Ratio`))  

WestFreeThrowPercentage20142015 <- c(mean(DAL20142015$`FreeThrow%`), mean(DEN20142015$`FreeThrow%`), mean(GSW20142015$`FreeThrow%`),mean(HOU20142015$`FreeThrow%`),mean(LAC20142015$`FreeThrow%`),mean(LAL20142015$`FreeThrow%`),mean(MEM20142015$`FreeThrow%`),mean(MIN20142015$`FreeThrow%`),mean(NOP20142015$`FreeThrow%`),mean(OKC20142015$`FreeThrow%`),mean(PHO20142015$`FreeThrow%`),mean(POR20142015$`FreeThrow%`),mean(SAC20142015$`FreeThrow%`),mean(SAS20142015$`FreeThrow%`),mean(UTA20142015$`FreeThrow%`))  

WestOpponentFreeThrowPercentage20142015 <- c(mean(DAL20142015$`OpponentFreeThrow%`), mean(DEN20142015$`OpponentFreeThrow%`), mean(GSW20142015$`OpponentFreeThrow%`),mean(HOU20142015$`OpponentFreeThrow%`),mean(LAC20142015$`OpponentFreeThrow%`),mean(LAL20142015$`OpponentFreeThrow%`),mean(MEM20142015$`OpponentFreeThrow%`),mean(MIN20142015$`OpponentFreeThrow%`),mean(NOP20142015$`OpponentFreeThrow%`),mean(OKC20142015$`OpponentFreeThrow%`),mean(PHO20142015$`OpponentFreeThrow%`),mean(POR20142015$`OpponentFreeThrow%`),mean(SAC20142015$`OpponentFreeThrow%`),mean(SAS20142015$`OpponentFreeThrow%`),mean(UTA20142015$`OpponentFreeThrow%`))  
Championship <- c("No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","No")
WesternConference20142015TeamsDf <- data.frame(WestTeams, WestWins20142015, WestThreePointMakes20142015, WestThreePointAttempts20142015, WestThreePointPercentage20142015, WestOpponent3PointPercentage20142015, WestFieldGoalPercentage20142015, WestOpponentFGpercentage20142015, WestFreeThrowPercentage20142015, WestOpponentFreeThrowPercentage20142015, WestAssistToTurnoverRatio20142015, Championship)

g1W2015 <- ggplot(WesternConference20142015TeamsDf, aes (x = WestThreePointMakes20142015, y = WestWins20142015, colour = WestTeams)) + geom_point() + ggtitle("2014-2015 Western Conference Teams: 3point Makes vs Wins")
g2W2015 <- ggplot(WesternConference20142015TeamsDf, aes (x = WestThreePointAttempts20142015, y = WestWins20142015, colour = WestTeams)) + geom_point() + ggtitle("2014-2015 Western Conference Teams: 3pointAttempts vs Wins")

g3W2015 <- ggplot(WesternConference20142015TeamsDf, aes (x = WestAssists20142015, y = WestWins20142015, colour = WestTeams)) + geom_point() + ggtitle("2014-2015 Western Conference Teams: Assists vs Wins")




# Calculate Wins for Eastern Conference Teams 2015 2016
ATLwins20152016 <- nba20152016season$Team == "ATL" & nba20152016season$WINorLOSS == "W"
sum(ATLwins20152016)
BOSwins20152016 <- nba20152016season$Team == "BOS" & nba20152016season$WINorLOSS == "W"
sum(BOSwins20152016)
BRKwins20152016 <- nba20152016season$Team == "BRK" & nba20152016season$WINorLOSS == "W"
sum(BRKwins20152016)
CHOwins20152016 <- nba20152016season$Team == "CHO" & nba20152016season$WINorLOSS == "W"
sum(CHOwins20152016)
CHIwins20152016 <- nba20152016season$Team == "CHI" & nba20152016season$WINorLOSS == "W"
sum(CHIwins20152016)
CLEwins20152016 <- nba20152016season$Team == "CLE" & nba20152016season$WINorLOSS == "W"
sum(CLEwins20152016)
DETwins20152016 <- nba20152016season$Team == "DET" & nba20152016season$WINorLOSS == "W"
sum(DETwins20152016)
INDwins20152016 <- nba20152016season$Team == "IND" & nba20152016season$WINorLOSS == "W"
sum(INDwins20152016)
MIAwins20152016 <- nba20152016season$Team == "MIA" & nba20152016season$WINorLOSS == "W"
sum(MIAwins20152016)
MILwins20152016 <- nba20152016season$Team == "MIL" & nba20152016season$WINorLOSS == "W"
sum(MILwins20152016)
NYKwins20152016 <- nba20152016season$Team == "NYK" & nba20152016season$WINorLOSS == "W"
sum(NYKwins20152016)
ORLwins20152016 <- nba20152016season$Team == "ORL" & nba20152016season$WINorLOSS == "W"
sum(ORLwins20152016)
PHIwins20152016 <- nba20152016season$Team == "PHI" & nba20152016season$WINorLOSS == "W"
sum(PHIwins20152016)
TORwins20152016 <- nba20152016season$Team == "TOR" & nba20152016season$WINorLOSS == "W"
sum(TORwins20152016)
WASwins20152016 <- nba20152016season$Team == "WAS" & nba20152016season$WINorLOSS == "W"
sum(WASwins20152016)

# Win's vector for Eastern Conference
EastWins20152016 <- c(sum(ATLwins20152016),sum(BOSwins20152016),sum(BRKwins20152016),sum(CHIwins20152016),sum(CHOwins20152016),sum(CLEwins20152016),sum(DETwins20152016),sum(INDwins20152016),sum(MIAwins20152016),sum(MILwins20152016),sum(NYKwins20152016),sum(ORLwins20152016),sum(PHIwins20152016),sum(TORwins20152016),sum(WASwins20152016))
EastWins20152016


# Atlanta 2015 2016
ATL20152016 <- nba20152016season[1:82,]

# Boston 2015 2016
BOS20152016 <- nba20152016season[83:164,]

#Brooklyn 2015 2016
BRK20152016 <- nba20152016season[165:246,]

# CHO 2015 2016
CHO20152016 <- nba20152016season[247:328,]
#  Chicago 2015 2016
CHI20152016 <- nba20152016season[329:410,]
# Cleveland 2015 2016
CLE20152016 <- nba20152016season[411:492,]
# DET 2015 2016
DET20152016 <- nba20152016season[657:738,]
# IND 2015 2016
IND20152016 <- nba20152016season[903:984,]
# MIA 2015 2016
MIA20152016 <- nba20152016season[1231:1312,]
# MIL 2015 2016 
MIL20152016 <- nba20152016season[1313:1394,]
# NYK 2015 2016
NYK20152016 <- nba20152016season[1559:1640,]
# ORL 2015 2016
ORL20152016 <- nba20152016season[1723:1804,]
# PHI 2015 2016
PHI20152016 <- nba20152016season[1805:1886,]
# TOR 2015 2016
TOR20152016 <- nba20152016season[2215:2296,]
# WAS 2015 2016
WAS20152016 <- nba20152016season[2379:2460,]

EastThreePointPercentage20152016 <- c(mean(ATL20152016$`3Point%`), mean(BOS20152016$`3Point%`), mean(BRK20152016$`3Point%`),mean(CHI20152016$`3Point%`),mean(CHO20152016$`3Point%`),mean(CLE20152016$`3Point%`),mean(DET20152016$`3Point%`),mean(IND20152016$`3Point%`),mean(MIA20152016$`3Point%`),mean(MIL20152016$`3Point%`),mean(NYK20152016$`3Point%`),mean(ORL20152016$`3Point%`),mean(PHI20152016$`3Point%`),mean(TOR20152016$`3Point%`),mean(WAS20152016$`3Point%`))    
EastThreePointAttempts20152016 <- c(mean(ATL20152016$`3PointShotsAttemted`), mean(BOS20152016$`3PointShotsAttemted`), mean(BRK20152016$`3PointShotsAttemted`),mean(CHI20152016$`3PointShotsAttemted`),mean(CHO20152016$`3PointShotsAttemted`),mean(CLE20152016$`3PointShotsAttemted`),mean(DET20152016$`3PointShotsAttemted`),mean(IND20152016$`3PointShotsAttemted`),mean(MIA20152016$`3PointShotsAttemted`),mean(MIL20152016$`3PointShotsAttemted`),mean(NYK20152016$`3PointShotsAttemted`),mean(ORL20152016$`3PointShotsAttemted`),mean(PHI20152016$`3PointShotsAttemted`),mean(TOR20152016$`3PointShotsAttemted`),mean(WAS20152016$`3PointShotsAttemted`))  
EastThreePointMakes20152016 <- c(mean(ATL20152016$`3PointShotsMade`), mean(BOS20152016$`3PointShotsMade`), mean(BRK20152016$`3PointShotsMade`),mean(CHI20152016$`3PointShotsMade`),mean(CHO20152016$`3PointShotsMade`),mean(CLE20152016$`3PointShotsMade`),mean(DET20152016$`3PointShotsMade`),mean(IND20152016$`3PointShotsMade`),mean(MIA20152016$`3PointShotsMade`),mean(MIL20152016$`3PointShotsMade`),mean(NYK20152016$`3PointShotsMade`),mean(ORL20152016$`3PointShotsMade`),mean(PHI20152016$`3PointShotsMade`),mean(TOR20152016$`3PointShotsMade`),mean(WAS20152016$`3PointShotsMade`))  
EastOpponent3PointPercentage20152016 <- c(mean(ATL20152016$`Opponent3Point%`), mean(BOS20152016$`Opponent3Point%`), mean(BRK20152016$`Opponent3Point%`),mean(CHI20152016$`Opponent3Point%`),mean(CHO20152016$`Opponent3Point%`),mean(CLE20152016$`Opponent3Point%`),mean(DET20152016$`Opponent3Point%`),mean(IND20152016$`Opponent3Point%`),mean(MIA20152016$`Opponent3Point%`),mean(MIL20152016$`Opponent3Point%`),mean(NYK20152016$`Opponent3Point%`),mean(ORL20152016$`Opponent3Point%`),mean(PHI20152016$`Opponent3Point%`),mean(TOR20152016$`Opponent3Point%`),mean(WAS20152016$`Opponent3Point%`))  


EastFieldGoalPercentage20152016 <- c(mean(ATL20152016$`FieldGoal%`), mean(BOS20152016$`FieldGoal%`), mean(BRK20152016$`FieldGoal%`),mean(CHI20152016$`FieldGoal%`),mean(CHO20152016$`FieldGoal%`),mean(CLE20152016$`FieldGoal%`),mean(DET20152016$`FieldGoal%`),mean(IND20152016$`FieldGoal%`),mean(MIA20152016$`FieldGoal%`),mean(MIL20152016$`FieldGoal%`),mean(NYK20152016$`FieldGoal%`),mean(ORL20152016$`FieldGoal%`),mean(PHI20152016$`FieldGoal%`),mean(TOR20152016$`FieldGoal%`),mean(WAS20152016$`FieldGoal%`))  

EastOpponentFGpercentage20152016 <- c(mean(ATL20152016$`OpponentFieldGoal%`), mean(BOS20152016$`OpponentFieldGoal%`), mean(BRK20152016$`OpponentFieldGoal%`),mean(CHI20152016$`OpponentFieldGoal%`),mean(CHO20152016$`OpponentFieldGoal%`),mean(CLE20152016$`OpponentFieldGoal%`),mean(DET20152016$`OpponentFieldGoal%`),mean(IND20152016$`OpponentFieldGoal%`),mean(MIA20152016$`OpponentFieldGoal%`),mean(MIL20152016$`OpponentFieldGoal%`),mean(NYK20152016$`OpponentFieldGoal%`),mean(ORL20152016$`OpponentFieldGoal%`),mean(PHI20152016$`OpponentFieldGoal%`),mean(TOR20152016$`OpponentFieldGoal%`),mean(WAS20152016$`OpponentFieldGoal%`))  

EastAssistToTurnoverRatio20152016 <- c(mean(ATL20152016$`Assist/Turnover Ratio`), mean(BOS20152016$`Assist/Turnover Ratio`), mean(BRK20152016$`Assist/Turnover Ratio`),mean(CHI20152016$`Assist/Turnover Ratio`),mean(CHO20152016$`Assist/Turnover Ratio`),mean(CLE20152016$`Assist/Turnover Ratio`),mean(DET20152016$`Assist/Turnover Ratio`),mean(IND20152016$`Assist/Turnover Ratio`),mean(MIA20152016$`Assist/Turnover Ratio`),mean(MIL20152016$`Assist/Turnover Ratio`),mean(NYK20152016$`Assist/Turnover Ratio`),mean(ORL20152016$`Assist/Turnover Ratio`),mean(PHI20152016$`Assist/Turnover Ratio`),mean(TOR20152016$`Assist/Turnover Ratio`),mean(WAS20152016$`Assist/Turnover Ratio`))  

EastFreeThrowPercentage20152016 <- c(mean(ATL20152016$`FreeThrow%`), mean(BOS20152016$`FreeThrow%`), mean(BRK20152016$`FreeThrow%`),mean(CHI20152016$`FreeThrow%`),mean(CHO20152016$`FreeThrow%`),mean(CLE20152016$`FreeThrow%`),mean(DET20152016$`FreeThrow%`),mean(IND20152016$`FreeThrow%`),mean(MIA20152016$`FreeThrow%`),mean(MIL20152016$`FreeThrow%`),mean(NYK20152016$`FreeThrow%`),mean(ORL20152016$`FreeThrow%`),mean(PHI20152016$`FreeThrow%`),mean(TOR20152016$`FreeThrow%`),mean(WAS20152016$`FreeThrow%`))  

EastOpponentFreeThrowPercentage20152016 <- c(mean(ATL20152016$`OpponentFreeThrow%`), mean(BOS20152016$`OpponentFreeThrow%`), mean(BRK20152016$`OpponentFreeThrow%`),mean(CHI20152016$`OpponentFreeThrow%`),mean(CHO20152016$`OpponentFreeThrow%`),mean(CLE20152016$`OpponentFreeThrow%`),mean(DET20152016$`OpponentFreeThrow%`),mean(IND20152016$`OpponentFreeThrow%`),mean(MIA20152016$`OpponentFreeThrow%`),mean(MIL20152016$`OpponentFreeThrow%`),mean(NYK20152016$`OpponentFreeThrow%`),mean(ORL20152016$`OpponentFreeThrow%`),mean(PHI20152016$`OpponentFreeThrow%`),mean(TOR20152016$`OpponentFreeThrow%`),mean(WAS20152016$`OpponentFreeThrow%`))  
Championship <- c("No","No","No","No","No","Yes","No","No","No","No","No","No","No","No","No")
EasternConference20152016TeamsDf <- data.frame(EastTeams, EastWins20152016, EastThreePointMakes20152016,EastThreePointAttempts20152016, EastThreePointPercentage20152016, EastOpponent3PointPercentage20152016, EastFieldGoalPercentage20152016, EastOpponentFGpercentage20152016, EastFreeThrowPercentage20152016, EastOpponentFreeThrowPercentage20152016, EastAssistToTurnoverRatio20152016, Championship)
g1E2016 <- ggplot(EasternConference20152016TeamsDf, aes (x = EastThreePointMakes20152016, y = EastWins20152016, colour = EastTeams)) + geom_point() + ggtitle("2015-2016 Eastern Conference Teams: 3point Makes vs Wins")
g2E2016 <- ggplot(EasternConference20152016TeamsDf, aes (x = EastThreePointAttempts20152016, y = EastWins20152016, colour = EastTeams)) + geom_point() + ggtitle("2015-2016 Eastern Conference Teams: 3pointAttempts vs Wins")

g3E2016 <- ggplot(EasternConference20152016TeamsDf, aes (x = EastAssists20152016, y = EastWins20152016, colour = EastTeams)) + geom_point() + ggtitle("2015-2016 Eastern Conference Teams: Assists vs Wins")





# Calculate Wins for Eastern Conference Teams each season
DALwins20152016 <- nba20152016season$Team == "DAL" & nba20152016season$WINorLOSS == "W"
sum(DALwins20152016)
DENwins20152016 <- nba20152016season$Team == "DEN" & nba20152016season$WINorLOSS == "W"
sum(DENwins20152016)
GSWwins20152016 <- nba20152016season$Team == "GSW" & nba20152016season$WINorLOSS == "W"
sum(GSWwins20152016)
HOUwins20152016 <- nba20152016season$Team == "HOU" & nba20152016season$WINorLOSS == "W"
sum(HOUwins20152016)
LACwins20152016 <- nba20152016season$Team == "LAC" & nba20152016season$WINorLOSS == "W"
sum(LACwins20152016)
LALwins20152016 <- nba20152016season$Team == "LAL" & nba20152016season$WINorLOSS == "W"
sum(LALwins20152016)
MEMwins20152016 <- nba20152016season$Team == "MEM" & nba20152016season$WINorLOSS == "W"
sum(MEMwins20152016)
MINwins20152016 <- nba20152016season$Team == "MIN" & nba20152016season$WINorLOSS == "W"
sum(MINwins20152016)
NOPwins20152016 <- nba20152016season$Team == "NOP" & nba20152016season$WINorLOSS == "W"
sum(NOPwins20152016)
OKCwins20152016 <- nba20152016season$Team == "OKC" & nba20152016season$WINorLOSS == "W"
sum(OKCwins20152016)
PHOwins20152016 <- nba20152016season$Team == "PHO" & nba20152016season$WINorLOSS == "W"
sum(PHOwins20152016)
PORwins20152016 <- nba20152016season$Team == "POR" & nba20152016season$WINorLOSS == "W"
sum(PORwins20152016)
SACwins20152016 <- nba20152016season$Team == "SAC" & nba20152016season$WINorLOSS == "W"
sum(SACwins20152016)
SASwins20152016 <- nba20152016season$Team == "SAS" & nba20152016season$WINorLOSS == "W"
sum(SASwins20152016)
UTAwins20152016 <- nba20152016season$Team == "UTA" & nba20152016season$WINorLOSS == "W"
sum(UTAwins20152016)

# Win's Vector for Western Conference
WestWins20152016 <- c(sum(DALwins20152016),sum(DENwins20152016),sum(GSWwins20152016),sum(HOUwins20152016),sum(LACwins20152016),sum(LALwins20152016),sum(MEMwins20152016),sum(MINwins20152016),sum(NOPwins20152016),sum(OKCwins20152016),sum(PHOwins20152016),sum(PORwins20152016),sum(SACwins20152016),sum(SASwins20152016),sum(UTAwins20152016))

# Dallas 2015 2016
DAL20152016 <- nba20152016season[493:574,]
# Denver 2015 2016
DEN20152016 <- nba20152016season[575:656,]
#Golden State 2015 2016
GSW20152016 <- nba20152016season[739:820,]
# Houston 2015 2016
HOU20152016 <- nba20152016season[821:902,]
# LA Clippers 2015 2016
LAC20152016 <- nba20152016season[985:1066,]
# LA Lakers 2015 2016
LAL20152016 <- nba20152016season[1067:1148,]
# Memphis 2015 2016
MEM20152016 <- nba20152016season[1149:1230,]
# Minnesota 2015 2016
MIN20152016 <- nba20152016season[1395:1476,]
# New Orleans 2015 2016 
NOP20152016 <- nba20152016season[1477:1558,]
# OKC 2015 2016
OKC20152016 <- nba20152016season[1641:1722,]
# PHO 2015 2016
PHO20152016 <- nba20152016season[1887:1968,]
# Portland 2015 2016
POR20152016 <- nba20152016season[1969:2050,]
# Sacramento 2015 2016
SAC20152016 <- nba20152016season[2051:2132,]
# San Antonio 2015 2016
SAS20152016 <- nba20152016season[2133:2214,]
# Utah 2015 2016
UTA20152016 <- nba20152016season[2297:2378,]

WestTeams <- c("DAL","DEN","GSW","HOU","LAC","LAL","MEM","MIN","NOP","OKC","PHO","POR","SAC","SAS","UTA")
WestThreePointPercentage20152016 <- c(mean(DAL20152016$`3Point%`), mean(DEN20152016$`3Point%`), mean(GSW20152016$`3Point%`),mean(HOU20152016$`3Point%`),mean(LAC20152016$`3Point%`),mean(LAL20152016$`3Point%`),mean(MEM20152016$`3Point%`),mean(MIN20152016$`3Point%`),mean(NOP20152016$`3Point%`),mean(OKC20152016$`3Point%`),mean(PHO20152016$`3Point%`),mean(POR20152016$`3Point%`),mean(SAC20152016$`3Point%`),mean(SAS20152016$`3Point%`),mean(UTA20152016$`3Point%`))

WestThreePointAttempts20152016 <- c(mean(DAL20152016$`3PointShotsAttemted`), mean(DEN20152016$`3PointShotsAttemted`), mean(GSW20152016$`3PointShotsAttemted`),mean(HOU20152016$`3PointShotsAttemted`),mean(LAC20152016$`3PointShotsAttemted`),mean(LAL20152016$`3PointShotsAttemted`),mean(MEM20152016$`3PointShotsAttemted`),mean(MIN20152016$`3PointShotsAttemted`),mean(NOP20152016$`3PointShotsAttemted`),mean(OKC20152016$`3PointShotsAttemted`),mean(PHO20152016$`3PointShotsAttemted`),mean(POR20152016$`3PointShotsAttemted`),mean(SAC20152016$`3PointShotsAttemted`),mean(SAS20152016$`3PointShotsAttemted`),mean(UTA20152016$`3PointShotsAttemted`))

WestThreePointMakes20152016 <- c(mean(DAL20152016$`3PointShotsMade`), mean(DEN20152016$`3PointShotsMade`), mean(GSW20152016$`3PointShotsMade`),mean(HOU20152016$`3PointShotsMade`),mean(LAC20152016$`3PointShotsMade`),mean(LAL20152016$`3PointShotsMade`),mean(MEM20152016$`3PointShotsMade`),mean(MIN20152016$`3PointShotsMade`),mean(NOP20152016$`3PointShotsMade`),mean(OKC20152016$`3PointShotsMade`),mean(PHO20152016$`3PointShotsMade`),mean(POR20152016$`3PointShotsMade`),mean(SAC20152016$`3PointShotsMade`),mean(SAS20152016$`3PointShotsMade`),mean(UTA20152016$`3PointShotsMade`))  
WestOpponent3PointPercentage20152016 <- c(mean(DAL20152016$`Opponent3Point%`), mean(DEN20152016$`Opponent3Point%`), mean(GSW20152016$`Opponent3Point%`),mean(HOU20152016$`Opponent3Point%`),mean(LAC20152016$`Opponent3Point%`),mean(LAL20152016$`Opponent3Point%`),mean(MEM20152016$`Opponent3Point%`),mean(MIN20152016$`Opponent3Point%`),mean(NOP20152016$`Opponent3Point%`),mean(OKC20152016$`Opponent3Point%`),mean(PHO20152016$`Opponent3Point%`),mean(POR20152016$`Opponent3Point%`),mean(SAC20152016$`Opponent3Point%`),mean(SAS20152016$`Opponent3Point%`),mean(UTA20152016$`Opponent3Point%`))  

WestFieldGoalPercentage20152016 <- c(mean(DAL20152016$`FieldGoal%`), mean(DEN20152016$`FieldGoal%`), mean(GSW20152016$`FieldGoal%`),mean(HOU20152016$`FieldGoal%`),mean(LAC20152016$`FieldGoal%`),mean(LAL20152016$`FieldGoal%`),mean(MEM20152016$`FieldGoal%`),mean(MIN20152016$`FieldGoal%`),mean(NOP20152016$`FieldGoal%`),mean(OKC20152016$`FieldGoal%`),mean(PHO20152016$`FieldGoal%`),mean(POR20152016$`FieldGoal%`),mean(SAC20152016$`FieldGoal%`),mean(SAS20152016$`FieldGoal%`),mean(UTA20152016$`FieldGoal%`))  

WestOpponentFGpercentage20152016 <- c(mean(DAL20152016$`OpponentFieldGoal%`), mean(DEN20152016$`OpponentFieldGoal%`), mean(GSW20152016$`OpponentFieldGoal%`),mean(HOU20152016$`OpponentFieldGoal%`),mean(LAC20152016$`OpponentFieldGoal%`),mean(LAL20152016$`OpponentFieldGoal%`),mean(MEM20152016$`OpponentFieldGoal%`),mean(MIN20152016$`OpponentFieldGoal%`),mean(NOP20152016$`OpponentFieldGoal%`),mean(OKC20152016$`OpponentFieldGoal%`),mean(PHO20152016$`OpponentFieldGoal%`),mean(POR20152016$`OpponentFieldGoal%`),mean(SAC20152016$`OpponentFieldGoal%`),mean(SAS20152016$`OpponentFieldGoal%`),mean(UTA20152016$`OpponentFieldGoal%`))  

WestAssistToTurnoverRatio20152016 <- c(mean(DAL20152016$`Assist/Turnover Ratio`), mean(DEN20152016$`Assist/Turnover Ratio`), mean(GSW20152016$`Assist/Turnover Ratio`),mean(HOU20152016$`Assist/Turnover Ratio`),mean(LAC20152016$`Assist/Turnover Ratio`),mean(LAL20152016$`Assist/Turnover Ratio`),mean(MEM20152016$`Assist/Turnover Ratio`),mean(MIN20152016$`Assist/Turnover Ratio`),mean(NOP20152016$`Assist/Turnover Ratio`),mean(OKC20152016$`Assist/Turnover Ratio`),mean(PHO20152016$`Assist/Turnover Ratio`),mean(POR20152016$`Assist/Turnover Ratio`),mean(SAC20152016$`Assist/Turnover Ratio`),mean(SAS20152016$`Assist/Turnover Ratio`),mean(UTA20152016$`Assist/Turnover Ratio`))  

WestFreeThrowPercentage20152016 <- c(mean(DAL20152016$`FreeThrow%`), mean(DEN20152016$`FreeThrow%`), mean(GSW20152016$`FreeThrow%`),mean(HOU20152016$`FreeThrow%`),mean(LAC20152016$`FreeThrow%`),mean(LAL20152016$`FreeThrow%`),mean(MEM20152016$`FreeThrow%`),mean(MIN20152016$`FreeThrow%`),mean(NOP20152016$`FreeThrow%`),mean(OKC20152016$`FreeThrow%`),mean(PHO20152016$`FreeThrow%`),mean(POR20152016$`FreeThrow%`),mean(SAC20152016$`FreeThrow%`),mean(SAS20152016$`FreeThrow%`),mean(UTA20152016$`FreeThrow%`))  

WestOpponentFreeThrowPercentage20152016 <- c(mean(DAL20152016$`OpponentFreeThrow%`), mean(DEN20152016$`OpponentFreeThrow%`), mean(GSW20152016$`OpponentFreeThrow%`),mean(HOU20152016$`OpponentFreeThrow%`),mean(LAC20152016$`OpponentFreeThrow%`),mean(LAL20152016$`OpponentFreeThrow%`),mean(MEM20152016$`OpponentFreeThrow%`),mean(MIN20152016$`OpponentFreeThrow%`),mean(NOP20152016$`OpponentFreeThrow%`),mean(OKC20152016$`OpponentFreeThrow%`),mean(PHO20152016$`OpponentFreeThrow%`),mean(POR20152016$`OpponentFreeThrow%`),mean(SAC20152016$`OpponentFreeThrow%`),mean(SAS20152016$`OpponentFreeThrow%`),mean(UTA20152016$`OpponentFreeThrow%`))  
Championship <- c("No","No","No","No","No","No","No","No","No","No","No","No","No","No","No")
WesternConference20152016TeamsDf <- data.frame(WestTeams, WestWins20152016, WestThreePointMakes20152016, WestThreePointAttempts20152016, WestThreePointPercentage20152016, WestOpponent3PointPercentage20152016, WestFieldGoalPercentage20152016, WestOpponentFGpercentage20152016, WestFreeThrowPercentage20152016, WestOpponentFreeThrowPercentage20152016, WestAssistToTurnoverRatio20152016, Championship)
g1W2016 <- ggplot(WesternConference20152016TeamsDf, aes (x = WestThreePointMakes20152016, y = WestWins20152016, colour = WestTeams)) + geom_point() + ggtitle("2015-2016 Western Conference Teams: 3point Makes vs Wins")
g2W2016 <- ggplot(WesternConference20152016TeamsDf, aes (x = WestThreePointAttempts20152016, y = WestWins20152016, colour = WestTeams)) + geom_point() + ggtitle("2015-2016 Western Conference Teams: 3pointAttempts vs Wins")

g3W2016 <- ggplot(WesternConference20152016TeamsDf, aes (x = WestAssists20152016, y = WestWins20152016, colour = WestTeams)) + geom_point() + ggtitle("2015-2016 Western Conference Teams: Assists vs Wins")


# Calculate Wins for Eastern Conference Teams 2016 2017
ATLwins20162017 <- nba20162017season$Team == "ATL" & nba20162017season$WINorLOSS == "W"
sum(ATLwins20162017)
BOSwins20162017 <- nba20162017season$Team == "BOS" & nba20162017season$WINorLOSS == "W"
sum(BOSwins20162017)
BRKwins20162017 <- nba20162017season$Team == "BRK" & nba20162017season$WINorLOSS == "W"
sum(BRKwins20162017)
CHOwins20162017 <- nba20162017season$Team == "CHO" & nba20162017season$WINorLOSS == "W"
sum(CHOwins20162017)
CHIwins20162017 <- nba20162017season$Team == "CHI" & nba20162017season$WINorLOSS == "W"
sum(CHIwins20162017)
CLEwins20162017 <- nba20162017season$Team == "CLE" & nba20162017season$WINorLOSS == "W"
sum(CLEwins20162017)
DETwins20162017 <- nba20162017season$Team == "DET" & nba20162017season$WINorLOSS == "W"
sum(DETwins20162017)
INDwins20162017 <- nba20162017season$Team == "IND" & nba20162017season$WINorLOSS == "W"
sum(INDwins20162017)
MIAwins20162017 <- nba20162017season$Team == "MIA" & nba20162017season$WINorLOSS == "W"
sum(MIAwins20162017)
MILwins20162017 <- nba20162017season$Team == "MIL" & nba20162017season$WINorLOSS == "W"
sum(MILwins20162017)
NYKwins20162017 <- nba20162017season$Team == "NYK" & nba20162017season$WINorLOSS == "W"
sum(NYKwins20162017)
ORLwins20162017 <- nba20162017season$Team == "ORL" & nba20162017season$WINorLOSS == "W"
sum(ORLwins20162017)
PHIwins20162017 <- nba20162017season$Team == "PHI" & nba20162017season$WINorLOSS == "W"
sum(PHIwins20162017)
TORwins20162017 <- nba20162017season$Team == "TOR" & nba20162017season$WINorLOSS == "W"
sum(TORwins20162017)
WASwins20162017 <- nba20162017season$Team == "WAS" & nba20162017season$WINorLOSS == "W"
sum(WASwins20162017)

# Win's vector for Eastern Conference
EastWins20162017 <- c(sum(ATLwins20162017),sum(BOSwins20162017),sum(BRKwins20162017),sum(CHIwins20162017),sum(CHOwins20162017),sum(CLEwins20162017),sum(DETwins20162017),sum(INDwins20162017),sum(MIAwins20162017),sum(MILwins20162017),sum(NYKwins20162017),sum(ORLwins20162017),sum(PHIwins20162017),sum(TORwins20162017),sum(WASwins20162017))
EastWins20162017


# Atlanta 2016 2017
ATL20162017 <- nba20162017season[1:82,]

# Boston 2016 2017
BOS20162017 <- nba20162017season[83:164,]

#Brooklyn 2016 2017
BRK20162017 <- nba20162017season[165:246,]

# CHO 2016 2017
CHO20162017 <- nba20162017season[247:328,]
#  Chicago 2016 2017
CHI20162017 <- nba20162017season[329:410,]
# Cleveland 2016 2017
CLE20162017 <- nba20162017season[411:492,]
# DET 2016 2017
DET20162017 <- nba20162017season[657:738,]
# IND 2016 2017
IND20162017 <- nba20162017season[903:984,]
# MIA 2016 2017
MIA20162017 <- nba20162017season[1231:1312,]
# MIL 2016 2017 
MIL20162017 <- nba20162017season[1313:1394,]
# NYK 2016 2017
NYK20162017 <- nba20162017season[1559:1640,]
# ORL 2016 2017
ORL20162017 <- nba20162017season[1723:1804,]
# PHI 2016 2017
PHI20162017 <- nba20162017season[1805:1886,]
# TOR 2016 2017
TOR20162017 <- nba20162017season[2215:2296,]
# WAS 2016 2017
WAS20162017 <- nba20162017season[2379:2460,]

EastThreePointPercentage20162017 <- c(mean(ATL20162017$`3Point%`), mean(BOS20162017$`3Point%`), mean(BRK20162017$`3Point%`),mean(CHI20162017$`3Point%`),mean(CHO20162017$`3Point%`),mean(CLE20162017$`3Point%`),mean(DET20162017$`3Point%`),mean(IND20162017$`3Point%`),mean(MIA20162017$`3Point%`),mean(MIL20162017$`3Point%`),mean(NYK20162017$`3Point%`),mean(ORL20162017$`3Point%`),mean(PHI20162017$`3Point%`),mean(TOR20162017$`3Point%`),mean(WAS20162017$`3Point%`))    
EastThreePointAttempts20162017 <- c(mean(ATL20162017$`3PointShotsAttemted`), mean(BOS20162017$`3PointShotsAttemted`), mean(BRK20162017$`3PointShotsAttemted`),mean(CHI20162017$`3PointShotsAttemted`),mean(CHO20162017$`3PointShotsAttemted`),mean(CLE20162017$`3PointShotsAttemted`),mean(DET20162017$`3PointShotsAttemted`),mean(IND20162017$`3PointShotsAttemted`),mean(MIA20162017$`3PointShotsAttemted`),mean(MIL20162017$`3PointShotsAttemted`),mean(NYK20162017$`3PointShotsAttemted`),mean(ORL20162017$`3PointShotsAttemted`),mean(PHI20162017$`3PointShotsAttemted`),mean(TOR20162017$`3PointShotsAttemted`),mean(WAS20162017$`3PointShotsAttemted`))  
EastThreePointMakes20162017 <- c(mean(ATL20162017$`3PointShotsMade`), mean(BOS20162017$`3PointShotsMade`), mean(BRK20162017$`3PointShotsMade`),mean(CHI20162017$`3PointShotsMade`),mean(CHO20162017$`3PointShotsMade`),mean(CLE20162017$`3PointShotsMade`),mean(DET20162017$`3PointShotsMade`),mean(IND20162017$`3PointShotsMade`),mean(MIA20162017$`3PointShotsMade`),mean(MIL20162017$`3PointShotsMade`),mean(NYK20162017$`3PointShotsMade`),mean(ORL20162017$`3PointShotsMade`),mean(PHI20162017$`3PointShotsMade`),mean(TOR20162017$`3PointShotsMade`),mean(WAS20162017$`3PointShotsMade`))  
EastOpponent3PointPercentage20162017 <- c(mean(ATL20162017$`Opponent3Point%`), mean(BOS20162017$`Opponent3Point%`), mean(BRK20162017$`Opponent3Point%`),mean(CHI20162017$`Opponent3Point%`),mean(CHO20162017$`Opponent3Point%`),mean(CLE20162017$`Opponent3Point%`),mean(DET20162017$`Opponent3Point%`),mean(IND20162017$`Opponent3Point%`),mean(MIA20162017$`Opponent3Point%`),mean(MIL20162017$`Opponent3Point%`),mean(NYK20162017$`Opponent3Point%`),mean(ORL20162017$`Opponent3Point%`),mean(PHI20162017$`Opponent3Point%`),mean(TOR20162017$`Opponent3Point%`),mean(WAS20162017$`Opponent3Point%`))  


EastFieldGoalPercentage20162017 <- c(mean(ATL20162017$`FieldGoal%`), mean(BOS20162017$`FieldGoal%`), mean(BRK20162017$`FieldGoal%`),mean(CHI20162017$`FieldGoal%`),mean(CHO20162017$`FieldGoal%`),mean(CLE20162017$`FieldGoal%`),mean(DET20162017$`FieldGoal%`),mean(IND20162017$`FieldGoal%`),mean(MIA20162017$`FieldGoal%`),mean(MIL20162017$`FieldGoal%`),mean(NYK20162017$`FieldGoal%`),mean(ORL20162017$`FieldGoal%`),mean(PHI20162017$`FieldGoal%`),mean(TOR20162017$`FieldGoal%`),mean(WAS20162017$`FieldGoal%`))  

EastOpponentFGpercentage20162017 <- c(mean(ATL20162017$`OpponentFieldGoal%`), mean(BOS20162017$`OpponentFieldGoal%`), mean(BRK20162017$`OpponentFieldGoal%`),mean(CHI20162017$`OpponentFieldGoal%`),mean(CHO20162017$`OpponentFieldGoal%`),mean(CLE20162017$`OpponentFieldGoal%`),mean(DET20162017$`OpponentFieldGoal%`),mean(IND20162017$`OpponentFieldGoal%`),mean(MIA20162017$`OpponentFieldGoal%`),mean(MIL20162017$`OpponentFieldGoal%`),mean(NYK20162017$`OpponentFieldGoal%`),mean(ORL20162017$`OpponentFieldGoal%`),mean(PHI20162017$`OpponentFieldGoal%`),mean(TOR20162017$`OpponentFieldGoal%`),mean(WAS20162017$`OpponentFieldGoal%`))  

EastAssistToTurnoverRatio20162017 <- c(mean(ATL20162017$`Assist/Turnover Ratio`), mean(BOS20162017$`Assist/Turnover Ratio`), mean(BRK20162017$`Assist/Turnover Ratio`),mean(CHI20162017$`Assist/Turnover Ratio`),mean(CHO20162017$`Assist/Turnover Ratio`),mean(CLE20162017$`Assist/Turnover Ratio`),mean(DET20162017$`Assist/Turnover Ratio`),mean(IND20162017$`Assist/Turnover Ratio`),mean(MIA20162017$`Assist/Turnover Ratio`),mean(MIL20162017$`Assist/Turnover Ratio`),mean(NYK20162017$`Assist/Turnover Ratio`),mean(ORL20162017$`Assist/Turnover Ratio`),mean(PHI20162017$`Assist/Turnover Ratio`),mean(TOR20162017$`Assist/Turnover Ratio`),mean(WAS20162017$`Assist/Turnover Ratio`))  

EastFreeThrowPercentage20162017 <- c(mean(ATL20162017$`FreeThrow%`), mean(BOS20162017$`FreeThrow%`), mean(BRK20162017$`FreeThrow%`),mean(CHI20162017$`FreeThrow%`),mean(CHO20162017$`FreeThrow%`),mean(CLE20162017$`FreeThrow%`),mean(DET20162017$`FreeThrow%`),mean(IND20162017$`FreeThrow%`),mean(MIA20162017$`FreeThrow%`),mean(MIL20162017$`FreeThrow%`),mean(NYK20162017$`FreeThrow%`),mean(ORL20162017$`FreeThrow%`),mean(PHI20162017$`FreeThrow%`),mean(TOR20162017$`FreeThrow%`),mean(WAS20162017$`FreeThrow%`))  

EastOpponentFreeThrowPercentage20162017 <- c(mean(ATL20162017$`OpponentFreeThrow%`), mean(BOS20162017$`OpponentFreeThrow%`), mean(BRK20162017$`OpponentFreeThrow%`),mean(CHI20162017$`OpponentFreeThrow%`),mean(CHO20162017$`OpponentFreeThrow%`),mean(CLE20162017$`OpponentFreeThrow%`),mean(DET20162017$`OpponentFreeThrow%`),mean(IND20162017$`OpponentFreeThrow%`),mean(MIA20162017$`OpponentFreeThrow%`),mean(MIL20162017$`OpponentFreeThrow%`),mean(NYK20162017$`OpponentFreeThrow%`),mean(ORL20162017$`OpponentFreeThrow%`),mean(PHI20162017$`OpponentFreeThrow%`),mean(TOR20162017$`OpponentFreeThrow%`),mean(WAS20162017$`OpponentFreeThrow%`))  
Championship <- c("No","No","No","No","No","No","No","No","No","No","No","No","No","No","No")
EasternConference20162017TeamsDf <- data.frame(EastTeams, EastWins20162017, EastThreePointMakes20162017,EastThreePointAttempts20162017, EastThreePointPercentage20162017, EastOpponent3PointPercentage20162017, EastFieldGoalPercentage20162017, EastOpponentFGpercentage20162017, EastFreeThrowPercentage20162017, EastOpponentFreeThrowPercentage20162017, EastAssistToTurnoverRatio20162017, Championship)
g1E2017 <- ggplot(EasternConference20162017TeamsDf, aes (x = EastThreePointMakes20162017, y = EastWins20162017, colour = EastTeams)) + geom_point() + ggtitle("2016-2017 Eastern Conference Teams: 3point Makes vs Wins")
g2E2017 <- ggplot(EasternConference20162017TeamsDf, aes (x = EastThreePointAttempts20162017, y = EastWins20162017, colour = EastTeams)) + geom_point() + ggtitle("2016-2017 Eastern Conference Teams: 3pointAttempts vs Wins")

g3E2017 <- ggplot(EasternConference20162017TeamsDf, aes (x = EastAssists20162017, y = EastWins20162017, colour = EastTeams)) + geom_point() + ggtitle("2016-2017 Eastern Conference Teams: Assists vs Wins")

# Calculate Wins for Eastern Conference Teams each season
DALwins20162017 <- nba20162017season$Team == "DAL" & nba20162017season$WINorLOSS == "W"
sum(DALwins20162017)
DENwins20162017 <- nba20162017season$Team == "DEN" & nba20162017season$WINorLOSS == "W"
sum(DENwins20162017)
GSWwins20162017 <- nba20162017season$Team == "GSW" & nba20162017season$WINorLOSS == "W"
sum(GSWwins20162017)
HOUwins20162017 <- nba20162017season$Team == "HOU" & nba20162017season$WINorLOSS == "W"
sum(HOUwins20162017)
LACwins20162017 <- nba20162017season$Team == "LAC" & nba20162017season$WINorLOSS == "W"
sum(LACwins20162017)
LALwins20162017 <- nba20162017season$Team == "LAL" & nba20162017season$WINorLOSS == "W"
sum(LALwins20162017)
MEMwins20162017 <- nba20162017season$Team == "MEM" & nba20162017season$WINorLOSS == "W"
sum(MEMwins20162017)
MINwins20162017 <- nba20162017season$Team == "MIN" & nba20162017season$WINorLOSS == "W"
sum(MINwins20162017)
NOPwins20162017 <- nba20162017season$Team == "NOP" & nba20162017season$WINorLOSS == "W"
sum(NOPwins20162017)
OKCwins20162017 <- nba20162017season$Team == "OKC" & nba20162017season$WINorLOSS == "W"
sum(OKCwins20162017)
PHOwins20162017 <- nba20162017season$Team == "PHO" & nba20162017season$WINorLOSS == "W"
sum(PHOwins20162017)
PORwins20162017 <- nba20162017season$Team == "POR" & nba20162017season$WINorLOSS == "W"
sum(PORwins20162017)
SACwins20162017 <- nba20162017season$Team == "SAC" & nba20162017season$WINorLOSS == "W"
sum(SACwins20162017)
SASwins20162017 <- nba20162017season$Team == "SAS" & nba20162017season$WINorLOSS == "W"
sum(SASwins20162017)
UTAwins20162017 <- nba20162017season$Team == "UTA" & nba20162017season$WINorLOSS == "W"
sum(UTAwins20162017)

# Win's Vector for Western Conference
WestWins20162017 <- c(sum(DALwins20162017),sum(DENwins20162017),sum(GSWwins20162017),sum(HOUwins20162017),sum(LACwins20162017),sum(LALwins20162017),sum(MEMwins20162017),sum(MINwins20162017),sum(NOPwins20162017),sum(OKCwins20162017),sum(PHOwins20162017),sum(PORwins20162017),sum(SACwins20162017),sum(SASwins20162017),sum(UTAwins20162017))

# Dallas 2016 2017
DAL20162017 <- nba20162017season[493:574,]
# Denver 2016 2017
DEN20162017 <- nba20162017season[575:656,]
#Golden State 2016 2017
GSW20162017 <- nba20162017season[739:820,]
# Houston 2016 2017
HOU20162017 <- nba20162017season[821:902,]
# LA Clippers 2016 2017
LAC20162017 <- nba20162017season[985:1066,]
# LA Lakers 2016 2017
LAL20162017 <- nba20162017season[1067:1148,]
# Memphis 2016 2017
MEM20162017 <- nba20162017season[1149:1230,]
# Minnesota 2016 2017
MIN20162017 <- nba20162017season[1395:1476,]
# New Orleans 2016 2017 
NOP20162017 <- nba20162017season[1477:1558,]
# OKC 2016 2017
OKC20162017 <- nba20162017season[1641:1722,]
# PHO 2016 2017
PHO20162017 <- nba20162017season[1887:1968,]
# Portland 2016 2017
POR20162017 <- nba20162017season[1969:2050,]
# Sacramento 2016 2017
SAC20162017 <- nba20162017season[2051:2132,]
# San Antonio 2016 2017
SAS20162017 <- nba20162017season[2133:2214,]
# Utah 2016 2017
UTA20162017 <- nba20162017season[2297:2378,]

WestTeams <- c("DAL","DEN","GSW","HOU","LAC","LAL","MEM","MIN","NOP","OKC","PHO","POR","SAC","SAS","UTA")
WestThreePointPercentage20162017 <- c(mean(DAL20162017$`3Point%`), mean(DEN20162017$`3Point%`), mean(GSW20162017$`3Point%`),mean(HOU20162017$`3Point%`),mean(LAC20162017$`3Point%`),mean(LAL20162017$`3Point%`),mean(MEM20162017$`3Point%`),mean(MIN20162017$`3Point%`),mean(NOP20162017$`3Point%`),mean(OKC20162017$`3Point%`),mean(PHO20162017$`3Point%`),mean(POR20162017$`3Point%`),mean(SAC20162017$`3Point%`),mean(SAS20162017$`3Point%`),mean(UTA20162017$`3Point%`))

WestThreePointAttempts20162017 <- c(mean(DAL20162017$`3PointShotsAttemted`), mean(DEN20162017$`3PointShotsAttemted`), mean(GSW20162017$`3PointShotsAttemted`),mean(HOU20162017$`3PointShotsAttemted`),mean(LAC20162017$`3PointShotsAttemted`),mean(LAL20162017$`3PointShotsAttemted`),mean(MEM20162017$`3PointShotsAttemted`),mean(MIN20162017$`3PointShotsAttemted`),mean(NOP20162017$`3PointShotsAttemted`),mean(OKC20162017$`3PointShotsAttemted`),mean(PHO20162017$`3PointShotsAttemted`),mean(POR20162017$`3PointShotsAttemted`),mean(SAC20162017$`3PointShotsAttemted`),mean(SAS20162017$`3PointShotsAttemted`),mean(UTA20162017$`3PointShotsAttemted`))

WestThreePointMakes20162017 <- c(mean(DAL20162017$`3PointShotsMade`), mean(DEN20162017$`3PointShotsMade`), mean(GSW20162017$`3PointShotsMade`),mean(HOU20162017$`3PointShotsMade`),mean(LAC20162017$`3PointShotsMade`),mean(LAL20162017$`3PointShotsMade`),mean(MEM20162017$`3PointShotsMade`),mean(MIN20162017$`3PointShotsMade`),mean(NOP20162017$`3PointShotsMade`),mean(OKC20162017$`3PointShotsMade`),mean(PHO20162017$`3PointShotsMade`),mean(POR20162017$`3PointShotsMade`),mean(SAC20162017$`3PointShotsMade`),mean(SAS20162017$`3PointShotsMade`),mean(UTA20162017$`3PointShotsMade`))  
WestOpponent3PointPercentage20162017 <- c(mean(DAL20162017$`Opponent3Point%`), mean(DEN20162017$`Opponent3Point%`), mean(GSW20162017$`Opponent3Point%`),mean(HOU20162017$`Opponent3Point%`),mean(LAC20162017$`Opponent3Point%`),mean(LAL20162017$`Opponent3Point%`),mean(MEM20162017$`Opponent3Point%`),mean(MIN20162017$`Opponent3Point%`),mean(NOP20162017$`Opponent3Point%`),mean(OKC20162017$`Opponent3Point%`),mean(PHO20162017$`Opponent3Point%`),mean(POR20162017$`Opponent3Point%`),mean(SAC20162017$`Opponent3Point%`),mean(SAS20162017$`Opponent3Point%`),mean(UTA20162017$`Opponent3Point%`))  

WestFieldGoalPercentage20162017 <- c(mean(DAL20162017$`FieldGoal%`), mean(DEN20162017$`FieldGoal%`), mean(GSW20162017$`FieldGoal%`),mean(HOU20162017$`FieldGoal%`),mean(LAC20162017$`FieldGoal%`),mean(LAL20162017$`FieldGoal%`),mean(MEM20162017$`FieldGoal%`),mean(MIN20162017$`FieldGoal%`),mean(NOP20162017$`FieldGoal%`),mean(OKC20162017$`FieldGoal%`),mean(PHO20162017$`FieldGoal%`),mean(POR20162017$`FieldGoal%`),mean(SAC20162017$`FieldGoal%`),mean(SAS20162017$`FieldGoal%`),mean(UTA20162017$`FieldGoal%`))  

WestOpponentFGpercentage20162017 <- c(mean(DAL20162017$`OpponentFieldGoal%`), mean(DEN20162017$`OpponentFiegldGoal%`), mean(GSW20162017$`OpponentFieldGoal%`),mean(HOU20162017$`OpponentFieldGoal%`),mean(LAC20162017$`OpponentFieldGoal%`),mean(LAL20162017$`OpponentFieldGoal%`),mean(MEM20162017$`OpponentFieldGoal%`),mean(MIN20162017$`OpponentFieldGoal%`),mean(NOP20162017$`OpponentFieldGoal%`),mean(OKC20162017$`OpponentFieldGoal%`),mean(PHO20162017$`OpponentFieldGoal%`),mean(POR20162017$`OpponentFieldGoal%`),mean(SAC20162017$`OpponentFieldGoal%`),mean(SAS20162017$`OpponentFieldGoal%`),mean(UTA20162017$`OpponentFieldGoal%`))  

WestAssistToTurnoverRatio20162017 <- c(mean(DAL20162017$`Assist/Turnover Ratio`), mean(DEN20162017$`Assist/Turnover Ratio`), mean(GSW20162017$`Assist/Turnover Ratio`),mean(HOU20162017$`Assist/Turnover Ratio`),mean(LAC20162017$`Assist/Turnover Ratio`),mean(LAL20162017$`Assist/Turnover Ratio`),mean(MEM20162017$`Assist/Turnover Ratio`),mean(MIN20162017$`Assist/Turnover Ratio`),mean(NOP20162017$`Assist/Turnover Ratio`),mean(OKC20162017$`Assist/Turnover Ratio`),mean(PHO20162017$`Assist/Turnover Ratio`),mean(POR20162017$`Assist/Turnover Ratio`),mean(SAC20162017$`Assist/Turnover Ratio`),mean(SAS20162017$`Assist/Turnover Ratio`),mean(UTA20162017$`Assist/Turnover Ratio`))  

WestFreeThrowPercentage20162017 <- c(mean(DAL20162017$`FreeThrow%`), mean(DEN20162017$`FreeThrow%`), mean(GSW20162017$`FreeThrow%`),mean(HOU20162017$`FreeThrow%`),mean(LAC20162017$`FreeThrow%`),mean(LAL20162017$`FreeThrow%`),mean(MEM20162017$`FreeThrow%`),mean(MIN20162017$`FreeThrow%`),mean(NOP20162017$`FreeThrow%`),mean(OKC20162017$`FreeThrow%`),mean(PHO20162017$`FreeThrow%`),mean(POR20162017$`FreeThrow%`),mean(SAC20162017$`FreeThrow%`),mean(SAS20162017$`FreeThrow%`),mean(UTA20162017$`FreeThrow%`))  

WestOpponentFreeThrowPercentage20162017 <- c(mean(DAL20162017$`OpponentFreeThrow%`), mean(DEN20162017$`OpponentFreeThrow%`), mean(GSW20162017$`OpponentFreeThrow%`),mean(HOU20162017$`OpponentFreeThrow%`),mean(LAC20162017$`OpponentFreeThrow%`),mean(LAL20162017$`OpponentFreeThrow%`),mean(MEM20162017$`OpponentFreeThrow%`),mean(MIN20162017$`OpponentFreeThrow%`),mean(NOP20162017$`OpponentFreeThrow%`),mean(OKC20162017$`OpponentFreeThrow%`),mean(PHO20162017$`OpponentFreeThrow%`),mean(POR20162017$`OpponentFreeThrow%`),mean(SAC20162017$`OpponentFreeThrow%`),mean(SAS20162017$`OpponentFreeThrow%`),mean(UTA20162017$`OpponentFreeThrow%`))  
Championship <- c("No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","No")
WesternConference20162017TeamsDf <- data.frame(WestTeams, WestWins20162017, WestThreePointMakes20162017, WestThreePointAttempts20162017, WestThreePointPercentage20162017, WestOpponent3PointPercentage20162017, WestFieldGoalPercentage20162017, WestOpponentFGpercentage20162017, WestFreeThrowPercentage20162017, WestOpponentFreeThrowPercentage20162017, WestAssistToTurnoverRatio20162017, Championship)

g1W2017 <- ggplot(WesternConference20162017TeamsDf, aes (x = WestThreePointMakes20162017, y = WestWins20162017, colour = WestTeams)) + geom_point() + ggtitle("2016-2017 Western Conference Teams: 3pointMakes vs Wins")
g2W2017 <- ggplot(WesternConference20162017TeamsDf, aes (x = WestThreePointAttempts20162017, y = WestWins20162017, colour = WestTeams)) + geom_point() + ggtitle("2016-2017 Western Conference Teams: 3pointAttempts vs Wins")

g3W2017 <- ggplot(WesternConference20162017TeamsDf, aes (x = WestAssists20162017, y = WestWins20162017, colour = WestTeams)) + geom_point() + ggtitle("2016-2017 Western Conference Teams: Assists vs Wins")




# Calculate Wins for Eastern Conference Teams 2017 2018
ATLwins20172018 <- nba20172018season$Team == "ATL" & nba20172018season$WINorLOSS == "W"
sum(ATLwins20172018)
BOSwins20172018 <- nba20172018season$Team == "BOS" & nba20172018season$WINorLOSS == "W"
sum(BOSwins20172018)
BRKwins20172018 <- nba20172018season$Team == "BRK" & nba20172018season$WINorLOSS == "W"
sum(BRKwins20172018)
CHOwins20172018 <- nba20172018season$Team == "CHO" & nba20172018season$WINorLOSS == "W"
sum(CHOwins20172018)
CHIwins20172018 <- nba20172018season$Team == "CHI" & nba20172018season$WINorLOSS == "W"
sum(CHIwins20172018)
CLEwins20172018 <- nba20172018season$Team == "CLE" & nba20172018season$WINorLOSS == "W"
sum(CLEwins20172018)
DETwins20172018 <- nba20172018season$Team == "DET" & nba20172018season$WINorLOSS == "W"
sum(DETwins20172018)
INDwins20172018 <- nba20172018season$Team == "IND" & nba20172018season$WINorLOSS == "W"
sum(INDwins20172018)
MIAwins20172018 <- nba20172018season$Team == "MIA" & nba20172018season$WINorLOSS == "W"
sum(MIAwins20172018)
MILwins20172018 <- nba20172018season$Team == "MIL" & nba20172018season$WINorLOSS == "W"
sum(MILwins20172018)
NYKwins20172018 <- nba20172018season$Team == "NYK" & nba20172018season$WINorLOSS == "W"
sum(NYKwins20172018)
ORLwins20172018 <- nba20172018season$Team == "ORL" & nba20172018season$WINorLOSS == "W"
sum(ORLwins20172018)
PHIwins20172018 <- nba20172018season$Team == "PHI" & nba20172018season$WINorLOSS == "W"
sum(PHIwins20172018)
TORwins20172018 <- nba20172018season$Team == "TOR" & nba20172018season$WINorLOSS == "W"
sum(TORwins20172018)
WASwins20172018 <- nba20172018season$Team == "WAS" & nba20172018season$WINorLOSS == "W"
sum(WASwins20172018)

# Win's vector for Eastern Conference
EastWins20172018 <- c(sum(ATLwins20172018),sum(BOSwins20172018),sum(BRKwins20172018),sum(CHIwins20172018),sum(CHOwins20172018),sum(CLEwins20172018),sum(DETwins20172018),sum(INDwins20172018),sum(MIAwins20172018),sum(MILwins20172018),sum(NYKwins20172018),sum(ORLwins20172018),sum(PHIwins20172018),sum(TORwins20172018),sum(WASwins20172018))
EastWins20172018


# Atlanta 2017 2018
ATL20172018 <- nba20172018season[1:82,]

# Boston 2017 2018
BOS20172018 <- nba20172018season[83:164,]

#Brooklyn 2017 2018
BRK20172018 <- nba20172018season[165:246,]

# CHO 2017 2018
CHO20172018 <- nba20172018season[247:328,]
#  Chicago 2017 2018
CHI20172018 <- nba20172018season[329:410,]
# Cleveland 2017 2018
CLE20172018 <- nba20172018season[411:492,]
# DET 2017 2018
DET20172018 <- nba20172018season[657:738,]
# IND 2017 2018
IND20172018 <- nba20172018season[903:984,]
# MIA 2017 2018
MIA20172018 <- nba20172018season[1231:1312,]
# MIL 2017 2018
MIL20172018 <- nba20172018season[1313:1394,]
# NYK 2017 2018
NYK20172018 <- nba20172018season[1559:1640,]
# ORL 2017 2018
ORL20172018 <- nba20172018season[1723:1804,]
# PHI 2017 2018
PHI20172018 <- nba20172018season[1805:1886,]
# TOR 2017 2018
TOR20172018 <- nba20172018season[2215:2296,]
# WAS 2017 2018
WAS20172018 <- nba20172018season[2379:2460,]

EastThreePointPercentage20172018 <- c(mean(ATL20172018$`3Point%`), mean(BOS20172018$`3Point%`), mean(BRK20172018$`3Point%`),mean(CHI20172018$`3Point%`),mean(CHO20172018$`3Point%`),mean(CLE20172018$`3Point%`),mean(DET20172018$`3Point%`),mean(IND20172018$`3Point%`),mean(MIA20172018$`3Point%`),mean(MIL20172018$`3Point%`),mean(NYK20172018$`3Point%`),mean(ORL20172018$`3Point%`),mean(PHI20172018$`3Point%`),mean(TOR20172018$`3Point%`),mean(WAS20172018$`3Point%`))    
EastThreePointAttempts20172018 <- c(mean(ATL20172018$`3PointShotsAttemted`), mean(BOS20172018$`3PointShotsAttemted`), mean(BRK20172018$`3PointShotsAttemted`),mean(CHI20172018$`3PointShotsAttemted`),mean(CHO20172018$`3PointShotsAttemted`),mean(CLE20172018$`3PointShotsAttemted`),mean(DET20172018$`3PointShotsAttemted`),mean(IND20172018$`3PointShotsAttemted`),mean(MIA20172018$`3PointShotsAttemted`),mean(MIL20172018$`3PointShotsAttemted`),mean(NYK20172018$`3PointShotsAttemted`),mean(ORL20172018$`3PointShotsAttemted`),mean(PHI20172018$`3PointShotsAttemted`),mean(TOR20172018$`3PointShotsAttemted`),mean(WAS20172018$`3PointShotsAttemted`))  
EastThreePointMakes20172018 <- c(mean(ATL20172018$`3PointShotsMade`), mean(BOS20172018$`3PointShotsMade`), mean(BRK20172018$`3PointShotsMade`),mean(CHI20172018$`3PointShotsMade`),mean(CHO20172018$`3PointShotsMade`),mean(CLE20172018$`3PointShotsMade`),mean(DET20172018$`3PointShotsMade`),mean(IND20172018$`3PointShotsMade`),mean(MIA20172018$`3PointShotsMade`),mean(MIL20172018$`3PointShotsMade`),mean(NYK20172018$`3PointShotsMade`),mean(ORL20172018$`3PointShotsMade`),mean(PHI20172018$`3PointShotsMade`),mean(TOR20172018$`3PointShotsMade`),mean(WAS20172018$`3PointShotsMade`))  
EastOpponent3PointPercentage20172018 <- c(mean(ATL20172018$`Opponent3Point%`), mean(BOS20172018$`Opponent3Point%`), mean(BRK20172018$`Opponent3Point%`),mean(CHI20172018$`Opponent3Point%`),mean(CHO20172018$`Opponent3Point%`),mean(CLE20172018$`Opponent3Point%`),mean(DET20172018$`Opponent3Point%`),mean(IND20172018$`Opponent3Point%`),mean(MIA20172018$`Opponent3Point%`),mean(MIL20172018$`Opponent3Point%`),mean(NYK20172018$`Opponent3Point%`),mean(ORL20172018$`Opponent3Point%`),mean(PHI20172018$`Opponent3Point%`),mean(TOR20172018$`Opponent3Point%`),mean(WAS20172018$`Opponent3Point%`))  


EastFieldGoalPercentage20172018 <- c(mean(ATL20172018$`FieldGoal%`), mean(BOS20172018$`FieldGoal%`), mean(BRK20172018$`FieldGoal%`),mean(CHI20172018$`FieldGoal%`),mean(CHO20172018$`FieldGoal%`),mean(CLE20172018$`FieldGoal%`),mean(DET20172018$`FieldGoal%`),mean(IND20172018$`FieldGoal%`),mean(MIA20172018$`FieldGoal%`),mean(MIL20172018$`FieldGoal%`),mean(NYK20172018$`FieldGoal%`),mean(ORL20172018$`FieldGoal%`),mean(PHI20172018$`FieldGoal%`),mean(TOR20172018$`FieldGoal%`),mean(WAS20172018$`FieldGoal%`))  

EastOpponentFGpercentage20172018 <- c(mean(ATL20172018$`OpponentFieldGoal%`), mean(BOS20172018$`OpponentFieldGoal%`), mean(BRK20172018$`OpponentFieldGoal%`),mean(CHI20172018$`OpponentFieldGoal%`),mean(CHO20172018$`OpponentFieldGoal%`),mean(CLE20172018$`OpponentFieldGoal%`),mean(DET20172018$`OpponentFieldGoal%`),mean(IND20172018$`OpponentFieldGoal%`),mean(MIA20172018$`OpponentFieldGoal%`),mean(MIL20172018$`OpponentFieldGoal%`),mean(NYK20172018$`OpponentFieldGoal%`),mean(ORL20172018$`OpponentFieldGoal%`),mean(PHI20172018$`OpponentFieldGoal%`),mean(TOR20172018$`OpponentFieldGoal%`),mean(WAS20172018$`OpponentFieldGoal%`))  

EastAssistToTurnoverRatio20172018 <- c(mean(ATL20172018$`Assist/Turnover Ratio`), mean(BOS20172018$`Assist/Turnover Ratio`), mean(BRK20172018$`Assist/Turnover Ratio`),mean(CHI20172018$`Assist/Turnover Ratio`),mean(CHO20172018$`Assist/Turnover Ratio`),mean(CLE20172018$`Assist/Turnover Ratio`),mean(DET20172018$`Assist/Turnover Ratio`),mean(IND20172018$`Assist/Turnover Ratio`),mean(MIA20172018$`Assist/Turnover Ratio`),mean(MIL20172018$`Assist/Turnover Ratio`),mean(NYK20172018$`Assist/Turnover Ratio`),mean(ORL20172018$`Assist/Turnover Ratio`),mean(PHI20172018$`Assist/Turnover Ratio`),mean(TOR20172018$`Assist/Turnover Ratio`),mean(WAS20172018$`Assist/Turnover Ratio`))  

EastFreeThrowPercentage20172018 <- c(mean(ATL20172018$`FreeThrow%`), mean(BOS20172018$`FreeThrow%`), mean(BRK20172018$`FreeThrow%`),mean(CHI20172018$`FreeThrow%`),mean(CHO20172018$`FreeThrow%`),mean(CLE20172018$`FreeThrow%`),mean(DET20172018$`FreeThrow%`),mean(IND20172018$`FreeThrow%`),mean(MIA20172018$`FreeThrow%`),mean(MIL20172018$`FreeThrow%`),mean(NYK20172018$`FreeThrow%`),mean(ORL20172018$`FreeThrow%`),mean(PHI20172018$`FreeThrow%`),mean(TOR20172018$`FreeThrow%`),mean(WAS20172018$`FreeThrow%`))  

EastOpponentFreeThrowPercentage20172018 <- c(mean(ATL20172018$`OpponentFreeThrow%`), mean(BOS20172018$`OpponentFreeThrow%`), mean(BRK20172018$`OpponentFreeThrow%`),mean(CHI20172018$`OpponentFreeThrow%`),mean(CHO20172018$`OpponentFreeThrow%`),mean(CLE20172018$`OpponentFreeThrow%`),mean(DET20172018$`OpponentFreeThrow%`),mean(IND20172018$`OpponentFreeThrow%`),mean(MIA20172018$`OpponentFreeThrow%`),mean(MIL20172018$`OpponentFreeThrow%`),mean(NYK20172018$`OpponentFreeThrow%`),mean(ORL20172018$`OpponentFreeThrow%`),mean(PHI20172018$`OpponentFreeThrow%`),mean(TOR20172018$`OpponentFreeThrow%`),mean(WAS20172018$`OpponentFreeThrow%`))  

EasternConference20172018TeamsDf <- data.frame(EastTeams, EastWins20172018, EastThreePointMakes20172018,EastThreePointAttempts20172018, EastThreePointPercentage20172018, EastOpponent3PointPercentage20172018, EastFieldGoalPercentage20172018, EastOpponentFGpercentage20172018, EastFreeThrowPercentage20172018, EastOpponentFreeThrowPercentage20172018, EastAssistToTurnoverRatio20172018)

ggplot(EasternConference20172018TeamsDf, aes (x = EastThreePointPercentage20172018, y = EastWins20172018, colour = EastTeams)) + geom_point() + ggtitle("2017-2018 Eastern Conference Teams: 3point% vs Wins")
ggplot(EasternConference20172018TeamsDf, aes (x = EastThreePointAttempts20172018, y = EastWins20172018, colour = EastTeams)) + geom_point() + ggtitle("2017-2018 Eastern Conference Teams: 3pointAttempts vs Wins")

ggplot(EasternConference20172018TeamsDf, aes (x = EastFieldGoalPercentage20172018, y = EastWins20172018, colour = EastTeams)) + geom_point() + ggtitle("2017-2018 Eastern Conference Teams: FieldGoal% vs Wins")
ggplot(EasternConference20172018TeamsDf, aes (x = EastOpponent3PointPercentage20172018, y = EastWins20172018, colour = EastTeams)) + geom_point() + ggtitle("2017-2018 Eastern Conference Teams: Opponent 3point% vs Wins")




# Calculate Wins for Western Conference Teams each season
DALwins20172018 <- nba20172018season$Team == "DAL" & nba20172018season$WINorLOSS == "W"
sum(DALwins20172018)
DENwins20172018 <- nba20172018season$Team == "DEN" & nba20172018season$WINorLOSS == "W"
sum(DENwins20172018)
GSWwins20172018 <- nba20172018season$Team == "GSW" & nba20172018season$WINorLOSS == "W"
sum(GSWwins20172018)
HOUwins20172018 <- nba20172018season$Team == "HOU" & nba20172018season$WINorLOSS == "W"
sum(HOUwins20172018)
LACwins20172018 <- nba20172018season$Team == "LAC" & nba20172018season$WINorLOSS == "W"
sum(LACwins20172018)
LALwins20172018 <- nba20172018season$Team == "LAL" & nba20172018season$WINorLOSS == "W"
sum(LALwins20172018)
MEMwins20172018 <- nba20172018season$Team == "MEM" & nba20172018season$WINorLOSS == "W"
sum(MEMwins20172018)
MINwins20172018 <- nba20172018season$Team == "MIN" & nba20172018season$WINorLOSS == "W"
sum(MINwins20172018)
NOPwins20172018 <- nba20172018season$Team == "NOP" & nba20172018season$WINorLOSS == "W"
sum(NOPwins20172018)
OKCwins20172018 <- nba20172018season$Team == "OKC" & nba20172018season$WINorLOSS == "W"
sum(OKCwins20172018)
PHOwins20172018 <- nba20172018season$Team == "PHO" & nba20172018season$WINorLOSS == "W"
sum(PHOwins20172018)
PORwins20172018 <- nba20172018season$Team == "POR" & nba20172018season$WINorLOSS == "W"
sum(PORwins20172018)
SACwins20172018 <- nba20172018season$Team == "SAC" & nba20172018season$WINorLOSS == "W"
sum(SACwins20172018)
SASwins20172018 <- nba20172018season$Team == "SAS" & nba20172018season$WINorLOSS == "W"
sum(SASwins20172018)
UTAwins20172018 <- nba20172018season$Team == "UTA" & nba20172018season$WINorLOSS == "W"
sum(UTAwins20172018)

# Win's Vector for Western Conference
WestWins20172018 <- c(sum(DALwins20172018),sum(DENwins20172018),sum(GSWwins20172018),sum(HOUwins20172018),sum(LACwins20172018),sum(LALwins20172018),sum(MEMwins20172018),sum(MINwins20172018),sum(NOPwins20172018),sum(OKCwins20172018),sum(PHOwins20172018),sum(PORwins20172018),sum(SACwins20172018),sum(SASwins20172018),sum(UTAwins20172018))
WestWins20172018

# Dallas 2017 2018
DAL20172018 <- nba20172018season[493:574,]
# Denver 2017 2018
DEN20172018 <- nba20172018season[575:656,]
#Golden State 2017 2018
GSW20172018 <- nba20172018season[739:820,]
# Houston 2017 2018
HOU20172018 <- nba20172018season[821:902,]
# LA Clippers 2017 2018
LAC20172018 <- nba20172018season[985:1066,]
# LA Lakers 2017 2018
LAL20172018 <- nba20172018season[1067:1148,]
# Memphis 2017 2018
MEM20172018 <- nba20172018season[1149:1230,]
# Minnesota 2017 2018
MIN20172018 <- nba20172018season[1395:1476,]
# New Orleans 2017 2018 
NOP20172018 <- nba20172018season[1477:1558,]
# OKC 2017 2018
OKC20172018 <- nba20172018season[1641:1722,]
# PHO 2017 2018
PHO20172018 <- nba20172018season[1887:1968,]
# Portland 2017 2018
POR20172018 <- nba20172018season[1969:2050,]
# Sacramento 2017 2018
SAC20172018 <- nba20172018season[2051:2132,]
# San Antonio 2017 2018
SAS20172018 <- nba20172018season[2133:2214,]
# Utah 2017 2018
UTA20172018 <- nba20172018season[2297:2378,]

WestTeams <- c("DAL","DEN","GSW","HOU","LAC","LAL","MEM","MIN","NOP","OKC","PHO","POR","SAC","SAS","UTA")

WestThreePointPercentage20172018 <- c(mean(DAL20172018$`3Point%`), mean(DEN20172018$`3Point%`), mean(GSW20172018$`3Point%`),mean(HOU20172018$`3Point%`),mean(LAC20172018$`3Point%`),mean(LAL20172018$`3Point%`),mean(MEM20172018$`3Point%`),mean(MIN20172018$`3Point%`),mean(NOP20172018$`3Point%`),mean(OKC20172018$`3Point%`),mean(PHO20172018$`3Point%`),mean(POR20172018$`3Point%`),mean(SAC20172018$`3Point%`),mean(SAS20172018$`3Point%`),mean(UTA20172018$`3Point%`))

WestThreePointAttempts20172018 <- c(mean(DAL20172018$`3PointShotsAttemted`), mean(DEN20172018$`3PointShotsAttemted`), mean(GSW20172018$`3PointShotsAttemted`),mean(HOU20172018$`3PointShotsAttemted`),mean(LAC20172018$`3PointShotsAttemted`),mean(LAL20172018$`3PointShotsAttemted`),mean(MEM20172018$`3PointShotsAttemted`),mean(MIN20172018$`3PointShotsAttemted`),mean(NOP20172018$`3PointShotsAttemted`),mean(OKC20172018$`3PointShotsAttemted`),mean(PHO20172018$`3PointShotsAttemted`),mean(POR20172018$`3PointShotsAttemted`),mean(SAC20172018$`3PointShotsAttemted`),mean(SAS20172018$`3PointShotsAttemted`),mean(UTA20172018$`3PointShotsAttemted`))

WestThreePointMakes20172018 <- c(mean(DAL20172018$`3PointShotsMade`), mean(DEN20172018$`3PointShotsMade`), mean(GSW20172018$`3PointShotsMade`),mean(HOU20172018$`3PointShotsMade`),mean(LAC20172018$`3PointShotsMade`),mean(LAL20172018$`3PointShotsMade`),mean(MEM20172018$`3PointShotsMade`),mean(MIN20172018$`3PointShotsMade`),mean(NOP20172018$`3PointShotsMade`),mean(OKC20172018$`3PointShotsMade`),mean(PHO20172018$`3PointShotsMade`),mean(POR20172018$`3PointShotsMade`),mean(SAC20172018$`3PointShotsMade`),mean(SAS20172018$`3PointShotsMade`),mean(UTA20172018$`3PointShotsMade`))  
WestOpponent3PointPercentage20172018 <- c(mean(DAL20172018$`Opponent3Point%`), mean(DEN20172018$`Opponent3Point%`), mean(GSW20172018$`Opponent3Point%`),mean(HOU20172018$`Opponent3Point%`),mean(LAC20172018$`Opponent3Point%`),mean(LAL20172018$`Opponent3Point%`),mean(MEM20172018$`Opponent3Point%`),mean(MIN20172018$`Opponent3Point%`),mean(NOP20172018$`Opponent3Point%`),mean(OKC20172018$`Opponent3Point%`),mean(PHO20172018$`Opponent3Point%`),mean(POR20172018$`Opponent3Point%`),mean(SAC20172018$`Opponent3Point%`),mean(SAS20172018$`Opponent3Point%`),mean(UTA20172018$`Opponent3Point%`))  

WestFieldGoalPercentage20172018 <- c(mean(DAL20172018$`FieldGoal%`), mean(DEN20172018$`FieldGoal%`), mean(GSW20172018$`FieldGoal%`),mean(HOU20172018$`FieldGoal%`),mean(LAC20172018$`FieldGoal%`),mean(LAL20172018$`FieldGoal%`),mean(MEM20172018$`FieldGoal%`),mean(MIN20172018$`FieldGoal%`),mean(NOP20172018$`FieldGoal%`),mean(OKC20172018$`FieldGoal%`),mean(PHO20172018$`FieldGoal%`),mean(POR20172018$`FieldGoal%`),mean(SAC20172018$`FieldGoal%`),mean(SAS20172018$`FieldGoal%`),mean(UTA20172018$`FieldGoal%`))  

WestOpponentFGpercentage20172018 <- c(mean(DAL20172018$`OpponentFieldGoal%`), mean(DEN20172018$`OpponentFieldGoal%`), mean(GSW20172018$`OpponentFieldGoal%`),mean(HOU20172018$`OpponentFieldGoal%`),mean(LAC20172018$`OpponentFieldGoal%`),mean(LAL20172018$`OpponentFieldGoal%`),mean(MEM20172018$`OpponentFieldGoal%`),mean(MIN20172018$`OpponentFieldGoal%`),mean(NOP20172018$`OpponentFieldGoal%`),mean(OKC20172018$`OpponentFieldGoal%`),mean(PHO20172018$`OpponentFieldGoal%`),mean(POR20172018$`OpponentFieldGoal%`),mean(SAC20172018$`OpponentFieldGoal%`),mean(SAS20172018$`OpponentFieldGoal%`),mean(UTA20172018$`OpponentFieldGoal%`))  

WestAssistToTurnoverRatio20172018 <- c(mean(DAL20172018$`Assist/Turnover Ratio`), mean(DEN20172018$`Assist/Turnover Ratio`), mean(GSW20172018$`Assist/Turnover Ratio`),mean(HOU20172018$`Assist/Turnover Ratio`),mean(LAC20172018$`Assist/Turnover Ratio`),mean(LAL20172018$`Assist/Turnover Ratio`),mean(MEM20172018$`Assist/Turnover Ratio`),mean(MIN20172018$`Assist/Turnover Ratio`),mean(NOP20172018$`Assist/Turnover Ratio`),mean(OKC20172018$`Assist/Turnover Ratio`),mean(PHO20172018$`Assist/Turnover Ratio`),mean(POR20172018$`Assist/Turnover Ratio`),mean(SAC20172018$`Assist/Turnover Ratio`),mean(SAS20172018$`Assist/Turnover Ratio`),mean(UTA20172018$`Assist/Turnover Ratio`))  

WestFreeThrowPercentage20172018 <- c(mean(DAL20172018$`FreeThrow%`), mean(DEN20172018$`FreeThrow%`), mean(GSW20172018$`FreeThrow%`),mean(HOU20172018$`FreeThrow%`),mean(LAC20172018$`FreeThrow%`),mean(LAL20172018$`FreeThrow%`),mean(MEM20172018$`FreeThrow%`),mean(MIN20172018$`FreeThrow%`),mean(NOP20172018$`FreeThrow%`),mean(OKC20172018$`FreeThrow%`),mean(PHO20172018$`FreeThrow%`),mean(POR20172018$`FreeThrow%`),mean(SAC20172018$`FreeThrow%`),mean(SAS20172018$`FreeThrow%`),mean(UTA20172018$`FreeThrow%`))  

WestOpponentFreeThrowPercentage20172018 <- c(mean(DAL20172018$`OpponentFreeThrow%`), mean(DEN20172018$`OpponentFreeThrow%`), mean(GSW20172018$`OpponentFreeThrow%`),mean(HOU20172018$`OpponentFreeThrow%`),mean(LAC20172018$`OpponentFreeThrow%`),mean(LAL20172018$`OpponentFreeThrow%`),mean(MEM20172018$`OpponentFreeThrow%`),mean(MIN20172018$`OpponentFreeThrow%`),mean(NOP20172018$`OpponentFreeThrow%`),mean(OKC20172018$`OpponentFreeThrow%`),mean(PHO20172018$`OpponentFreeThrow%`),mean(POR20172018$`OpponentFreeThrow%`),mean(SAC20172018$`OpponentFreeThrow%`),mean(SAS20172018$`OpponentFreeThrow%`),mean(UTA20172018$`OpponentFreeThrow%`))  

WesternConference20172018TeamsDf <- data.frame(WestTeams, WestWins20172018, WestThreePointMakes20172018, WestThreePointAttempts20172018, WestThreePointPercentage20172018, WestOpponent3PointPercentage20172018, WestFieldGoalPercentage20172018, WestOpponentFGpercentage20172018, WestFreeThrowPercentage20172018, WestOpponentFreeThrowPercentage20172018, WestAssistToTurnoverRatio20172018)

ggplot(WesternConference20172018TeamsDf, aes (x = WestThreePointPercentage20172018, y = WestWins20172018, colour = WestTeams)) + geom_point() + ggtitle("2017-2018 Western Conference Teams: 3point% vs Wins")
ggplot(WesternConference20172018TeamsDf, aes (x = WestThreePointAttempts20172018, y = WestWins20172018, colour = WestTeams)) + geom_point() + ggtitle("2017-2018 Western Conference Teams: 3pointAttempts vs Wins")

ggplot(WesternConference20172018TeamsDf, aes (x = WestFreeThrowPercentage20172018, y = WestWins20172018, colour = WestTeams)) + geom_point() + ggtitle("2017-2018 Western Conference Teams: FreeThrow% vs Wins")
ggplot(WesternConference20172018TeamsDf, aes (x = WestOpponent3PointPercentage20172018, y = WestWins20172018, colour = WestTeams)) + geom_point() + ggtitle("2017-2018 Western Conference Teams: Opponent 3point% vs Wins")
ggplot(WesternConference20172018TeamsDf, aes (x = WestTeams, y = WestThreePointPercentage20172018, colour = WestWins20172018)) + geom_point() 

# decision trees

dt_model <- rpart(WINorLOSS ~ ., nba20142015season, method = "class")
fancyRpartPlot(dt_model)

NBA_Teams <- c(EastTeams, WestTeams)
NBA_Teams
View(WesternConference20142015TeamsDf)
Wins20142015 <- c(EastWins20142015, WestWins20142015)
Wins20142015
Wins20152016 <- c(EastWins20152016, WestWins20152016)
Wins20162017 <- c(EastWins20162017, WestWins20162017)
ThreePointMakes20142015 <- c(EastThreePointMakes20142015, WestThreePointMakes20142015)
ThreePointMakes20152016 <- c(EastThreePointMakes20152016, WestThreePointMakes20152016)
ThreePointMakes20162017 <- c(EastThreePointMakes20162017, WestThreePointMakes20162017)
ThreePointAttempts20142015 <- c(EastThreePointAttempts20142015, WestThreePointAttempts20142015)
ThreePointAttempts20152016 <- c(EastThreePointAttempts20152016, WestThreePointAttempts20152016)
ThreePointAttempts20162017 <- c(EastThreePointAttempts20162017, WestThreePointAttempts20162017)
ThreePointPercentage20142015 <- c(EastThreePointPercentage20142015, WestThreePointPercentage20142015)
ThreePointPercentage20152016 <- c(EastThreePointPercentage20152016, WestThreePointPercentage20152016)
ThreePointPercentage20162017 <- c(EastThreePointPercentage20162017, WestThreePointPercentage20162017)
OpponentThreePointPerecentage20142015 <- c(EastOpponent3PointPercentage20142015, WestOpponent3PointPercentage20142015)
OpponentThreePointPerecentage20152016 <- c(EastOpponent3PointPercentage20152016, WestOpponent3PointPercentage20152016)
OpponentThreePointPerecentage20162017 <- c(EastOpponent3PointPercentage20162017, WestOpponent3PointPercentage20162017)
FieldGoalPercentage20142015 <- c(EastFieldGoalPercentage20142015, WestFieldGoalPercentage20142015)
FieldGoalPercentage20152016 <- c(EastFieldGoalPercentage20152016, WestFieldGoalPercentage20152016)
FieldGoalPercentage20162017 <- c(EastFieldGoalPercentage20162017, WestFieldGoalPercentage20162017)
OpponentFieldGoalPercentage20142015 <- c(EastOpponentFGpercentage20142015, WestOpponentFGpercentage20142015)
OpponentFieldGoalPercentage20152016 <- c(EastOpponentFGpercentage20152016, WestOpponentFGpercentage20152016)
OpponentFieldGoalPercentage20162017 <- c(EastOpponentFGpercentage20162017, WestOpponentFGpercentage20162017)
FreeThrowPercentage20142015 <- c(EastFreeThrowPercentage20142015, WestFreeThrowPercentage20142015)
FreeThrowPercentage20152016 <- c(EastFreeThrowPercentage20152016, WestFreeThrowPercentage20152016)
FreeThrowPercentage20162017 <- c(EastFreeThrowPercentage20162017, WestFreeThrowPercentage20162017)
OpponentFreeThrowPercentage20142015 <- c(EastOpponentFreeThrowPercentage20142015, WestOpponentFreeThrowPercentage20142015)
OpponentFreeThrowPercentage20152016 <- c(EastOpponentFreeThrowPercentage20152016, WestOpponentFreeThrowPercentage20152016)
OpponentFreeThrowPercentage20162017 <- c(EastOpponentFreeThrowPercentage20162017, WestOpponentFreeThrowPercentage20162017)
AssistToTurnoverRatio20142015 <- c(EastAssistToTurnoverRatio20142015, WestAssistToTurnoverRatio20142015)
AssistToTurnoverRatio20152016 <- c(EastAssistToTurnoverRatio20152016, WestAssistToTurnoverRatio20152016)
AssistToTurnoverRatio20162017 <- c(EastAssistToTurnoverRatio20162017, WestAssistToTurnoverRatio20162017)
Championships <- c("No","No","No","No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","No")


training_data <- data.frame(NBA_Teams,Wins20142015,Wins20152016,Wins20162017, ThreePointMakes20142015, ThreePointMakes20152016, ThreePointMakes20162017, ThreePointAttempts20142015, ThreePointAttempts20152016, ThreePointAttempts20162017, ThreePointPercentage20142015, ThreePointPercentage20152016, ThreePointPercentage20162017, OpponentThreePointPerecentage20142015, OpponentThreePointPerecentage20152016, OpponentFreeThrowPercentage20162017, FieldGoalPercentage20142015, FieldGoalPercentage20152016, FieldGoalPercentage20162017, OpponentFieldGoalPercentage20142015, OpponentFieldGoalPercentage20152016, OpponentFieldGoalPercentage20162017, FreeThrowPercentage20142015, FreeThrowPercentage20152016, FreeThrowPercentage20162017, OpponentFreeThrowPercentage20142015, OpponentFreeThrowPercentage20152016, OpponentFreeThrowPercentage20162017, AssistToTurnoverRatio20142015, AssistToTurnoverRatio20152016, AssistToTurnoverRatio20162017, Championships)
training_data
str(training_data)
summary(training_data)
ggplot(training_data, aes(x = ThreePointAttempts20142015, y = Wins20142015, colour = NBA_Teams)) + geom_point() + ggtitle("2014-2015 NBA 3 Point Attempts vs Wins")
ggplot(WesternConference20172018TeamsDf, aes (x = WestThreePointAttempts20172018, y = WestWins20172018, colour = WestTeams)) + geom_point() + ggtitle("2017-2018 Western Conference Teams: 3pointAttempts vs Wins")


Wins20172018 <- c(EastWins20172018, WestWins20172018)
ThreePointMakes20172018 <- c(EastThreePointMakes20172018, WestThreePointMakes20172018)
ThreePointAttempts20172018 <- c(EastThreePointAttempts20172018, WestThreePointAttempts20172018)
ThreePointPercentage20172018 <- c(EastThreePointPercentage20172018, WestThreePointPercentage20172018)
OpponentThreePointPercentage20172018 <- c(EastOpponent3PointPercentage20172018, WestOpponent3PointPercentage20172018)
FieldGoalPerecentage20172018 <- c(EastFieldGoalPercentage20172018, WestFieldGoalPercentage20172018)
OpponentFieldGoalPercentage20172018 <- c(EastOpponent3PointPercentage20172018, WestOpponent3PointPercentage20172018)
FreeThrowPercentage20172018 <- c(EastFreeThrowPercentage20172018, WestFreeThrowPercentage20172018)
OpponentFreeThrowPercentage20172018 <- c(EastOpponentFreeThrowPercentage20172018, WestOpponentFreeThrowPercentage20172018)
AssistToTurnoverRatio20172018 <- c(EastAssistToTurnoverRatio20172018, WestAssistToTurnoverRatio20172018)
Championships <- c("No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No")

testing_data <- data.frame(NBA_Teams, Wins20172018, ThreePointMakes20172018, ThreePointAttempts20172018, ThreePointPercentage20172018, OpponentFieldGoalPercentage20172018, OpponentFreeThrowPercentage20172018, OpponentThreePointPercentage20172018,FieldGoalPerecentage20172018,FreeThrowPercentage20172018,AssistToTurnoverRatio20172018,Championships)

str(training_data)

decision_tree <- rpart(WINorLOSS ~ FieldGoalPercentage + AssistToTurnoverRatio + ThreePointPercentage + OpponentFieldGoalPercentage + OpponentThreePointPercentage, data = nba_stats, method = "class")
fancyRpartPlot(decision_tree)
decision_tree2 <- rpart(WINorLOSS ~ ., data = nba_stats, method = "class")
fancyRpartPlot(decision_tree2)

decision_tree3 <- rpart(WINorLOSS ~ FieldGoalPercentage + AssistToTurnoverRatio + ThreePointPercentage + OpponentFieldGoalPercentage + OpponentThreePointPercentage, data = training_data, method = "class")
fancyRpartPlot(decision_tree3)

decision_tree4 <- rpart(WINorLOSS ~ ., data = training_data, method = "class")
fancyRpartPlot(decision_tree4)


my_prediction <- predict(decision_tree3, testing_data, type = "class")
confusionMatrix(testing_data$WINorLOSS, my_prediction)

my_prediction2 <- predict(decision_tree4, testing_data, type = "class")
confusionMatrix(testing_data$WINorLOSS, my_prediction2)
# naive bayes
nb_model <- naiveBayes(WINorLOSS ~ ., training_data, method = "class")
nb_pred <- predict(decision_tree4, testing_data, type = "class")

nb_pred
#table
table(nb_pred)
nb_pred

confusionMatrix(testing_data$WINorLOSS, nb_pred)

NBclassifier <- naive_bayes(Championships ~ NBA_Teams+ThreePointMakes20142015+ThreePointMakes20152016+ThreePointMakes20162017+ThreePointAttempts20142015+ThreePointAttempts20152016+ThreePointAttempts20162017,  data = training_data)
print(NBclassifier)

testPred <- predict(NBclassifier, data=testing_data, type="prob")
testPred
confusionMatrix(testing_data$Championships, testPred)
help(predict)


  
  
  