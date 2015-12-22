library(dplyr)
QB_2015 <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week1-4QB.csv",stringsAsFactors=FALSE)
RB_2015 <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week1-4-RB.csv",stringsAsFactors=FALSE)
WR_2015 <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week1-4-WR.csv",stringsAsFactors=FALSE)
TE_2015 <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week1-4-TE.csv",stringsAsFactors=FALSE)
D_2015 <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week1-4-D.csv",stringsAsFactors=FALSE)
TOTALD <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-Total_DEF.csv", stringsAsFactors=FALSE)
TOTALO <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-Total_OFF.csv",stringsAsFactors=FALSE)
DownsO <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-DOWNS_OFF.csv",stringsAsFactors=FALSE)
DownsD <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-DOWNS_DEF.csv",stringsAsFactors=FALSE)
GiveTake <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-GIVE_TAKE.csv",stringsAsFactors=FALSE)
PassingD <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-PASSING_DEF.csv",stringsAsFactors=FALSE)
PassingO <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-PASSING_OFF.csv",stringsAsFactors=FALSE)
PuntingD <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-PUNTING_DEF.csv",stringsAsFactors=FALSE)
PuntingO <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-PUNTING_OFF.csv",stringsAsFactors=FALSE)
ReceivingD <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-RECEIVING_DEF.csv",stringsAsFactors=FALSE)
ReceivingO <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/Week5/Week4-RECEIVING_OFF.csv",stringsAsFactors=FALSE)
Madden_Player <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/nfl/Madden_Player.csv", stringsAsFactors=FALSE)
Madden_Team <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/nfl/Madden_Team.csv", stringsAsFactors=FALSE)
Abbreviation <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/nfl/Abbreviation.csv", stringsAsFactors=FALSE)

######################################################################################################

MaddenQB <- select(Madden_Player,Player,OVR,Speed,Awareness,Throw.Power,Throw.Accuracy.Short,
                   Throw.Accuracy.Mid,Throw.Accuracy.Deep,Injury,Stamina,Toughness,Play.Action,Throw.On.The.Run)
QB_2015 <- left_join(QB_2015,MaddenQB,by="Player")
TOTALD <- rename(TOTALD,Opp=TEAM,D.Yds.G = YDS.G,D.P.YDS.G=P.YDS.G,D.R.YDS.G=R.YDS.G,D.PTS.G=PTS.G)
TOTALD.QB <- select(TOTALD,Opp,D.Yds.G,D.P.YDS.G,D.R.YDS.G,D.PTS.G)
QB_2015 <- left_join(QB_2015,TOTALD.QB,by="Opp")
PassD <- rename(PassingD,Opp=TEAM,INT.D=INT,D.TD=TD,D.TD1=TD.1)
PassD <- select(PassD,Opp,INT.D,D.TD,D.TD1,SOLO,AST,SACK,PD,LONG,FF)
QB_2015 <- left_join(QB_2015,PassD,by="Opp")
TEAM.QB <- mutate(Madden_Team,Player = paste(Location,Nickname))
TEAM.QB <- left_join(TEAM.QB,Abbreviation,by="Player") %>%
  select(TEAM.OVR,Offense,Defense,Abbreviation) %>%
  rename(Opp=Abbreviation)
QB_2015 <- left_join(QB_2015,TEAM.QB,by="Opp")
RECD <- select(ReceivingD,TEAM,YDS.A,TD.G,YDS.G) %>%
  rename(Opp = TEAM)
QB_2015 <- left_join(QB_2015,RECD,by="Opp")
QB_2015 <- QB_2015[complete.cases(QB_2015),]
QBModel <- lm(Fantasy.Points ~ OVR + Speed + Awareness + Throw.Power + Throw.Accuracy.Short + 
                     Throw.Accuracy.Mid + Throw.Accuracy.Deep + Injury + Stamina + Toughness + 
                     Play.Action + Throw.On.The.Run + D.Yds.G + D.P.YDS.G + D.R.YDS.G + D.PTS.G + 
                     INT.D + D.TD + D.TD1 + SOLO + AST + SACK + PD + LONG + FF + TEAM.OVR + Offense +
                     Defense + YDS.A + TD.G + YDS.G + 0,data=QB_2015)
QBMODE <- step(QBModel,direction="both")

######################################################################################################

MaddenRB <- select(Madden_Player,Player,OVR,Speed,Agility,Acceleration,Awareness,Catching,Carrying,
                   Jumping,Injury,Stamina,Toughness,Trucking,Elusiveness,Ball.Carrier.Vision,Stiff.Arm,
                   Spin.Move,Juke.Move,Spectactular.Catch,Catch.In.Traffic,Route.Running,Release,Weight,Kick.Return)
RB_2015 <- left_join(RB_2015,MaddenRB,by="Player")
TOTALD.RB <- select(TOTALD,Opp,D.Yds.G,D.P.YDS.G,D.R.YDS.G,D.PTS.G)
RB_2015 <- left_join(RB_2015,TOTALD.RB,by="Opp")
TEAM.RB <- mutate(Madden_Team,Player = paste(Location,Nickname))
TEAM.RB <- left_join(TEAM.RB,Abbreviation,by="Player") %>%
  select(TEAM.OVR,Offense,Defense,Abbreviation) %>%
  rename(Opp=Abbreviation)
RB_2015 <- left_join(RB_2015,TEAM.RB,by="Opp")
RB_2015 <- left_join(RB_2015,PassD,by="Opp")
RB_2015 <- left_join(RB_2015,RECD,by="Opp")
RB_2015 <- RB_2015[complete.cases(RB_2015),]
RBModel <- lm(Fantasy.Points ~ Player + OVR + Speed + Agility + Acceleration + Awareness + Catching + Carrying + Jumping +
                Injury + Stamina + Toughness + Trucking + Elusiveness + Ball.Carrier.Vision + Stiff.Arm + Spin.Move +
                Juke.Move + Spectactular.Catch + Catch.In.Traffic + Route.Running + Release + Weight + D.Yds.G + 
                D.P.YDS.G + D.R.YDS.G + D.PTS.G + TEAM.OVR + Offense + Defense + Kick.Return + INT.D + D.TD + 
                D.TD1 + SOLO + AST + SACK + PD + LONG + FF + YDS.A + TD.G + YDS.G + 0, data= RB_2015)
RBMODE <- step(RBModel,direction="both")

######################################################################################################

MaddenWR <- select(Madden_Player,Player,OVR,Speed,Agility,Acceleration,Awareness,Catching,Carrying,
                   Jumping,Injury,Stamina,Toughness,Trucking,Elusiveness,Ball.Carrier.Vision,Stiff.Arm,
                   Spin.Move,Juke.Move,Spectactular.Catch,Catch.In.Traffic,Route.Running,Release,Weight,Kick.Return)
WR_2015 <- left_join(WR_2015,MaddenWR,by="Player")
TOTALD.WR <- select(TOTALD,Opp,D.Yds.G,D.P.YDS.G,D.R.YDS.G,D.PTS.G)
WR_2015 <- left_join(WR_2015,TOTALD.WR,by="Opp")
TEAM.WR <- mutate(Madden_Team,Player = paste(Location,Nickname))
TEAM.WR <- left_join(TEAM.WR,Abbreviation,by="Player") %>%
  select(TEAM.OVR,Offense,Defense,Abbreviation) %>%
  rename(Opp=Abbreviation)
WR_2015 <- left_join(WR_2015,TEAM.WR,by="Opp")
WR_2015 <- left_join(WR_2015,PassD,by="Opp")
WR_2015 <- left_join(WR_2015,RECD,by="Opp")
WR_2015 <- WR_2015[complete.cases(WR_2015),]
WRModel <- lm(Fantasy.Points ~ Player + OVR + Speed + Agility + Acceleration + Awareness + Catching + Carrying + Jumping +
                Injury + Stamina + Toughness + Trucking + Elusiveness + Ball.Carrier.Vision + Stiff.Arm + Spin.Move +
                Juke.Move + Spectactular.Catch + Catch.In.Traffic + Route.Running + Release + Weight + D.Yds.G + 
                D.P.YDS.G + D.R.YDS.G + D.PTS.G + TEAM.OVR + Offense + Defense + Kick.Return + INT.D + D.TD + 
                D.TD1 + SOLO + AST + SACK + PD + LONG + FF + YDS.A + TD.G + YDS.G + 0, data= WR_2015)
WRMODE <- step(WRModel,direction="both")

######################################################################################################

MaddenTE <- select(Madden_Player,Player,OVR,Speed,Agility,Acceleration,Awareness,Catching,Carrying,
                   Jumping,Injury,Stamina,Toughness,Trucking,Elusiveness,Ball.Carrier.Vision,Stiff.Arm,
                   Spin.Move,Juke.Move,Spectactular.Catch,Catch.In.Traffic,Route.Running,Release,Weight,Kick.Return)
TE_2015 <- left_join(TE_2015,MaddenTE,by="Player")
TOTALD.TE <- select(TOTALD,Opp,D.Yds.G,D.P.YDS.G,D.R.YDS.G,D.PTS.G)
TE_2015 <- left_join(TE_2015,TOTALD.TE,by="Opp")
TEAM.TE <- mutate(Madden_Team,Player = paste(Location,Nickname))
TEAM.TE <- left_join(TEAM.TE,Abbreviation,by="Player") %>%
  select(TEAM.OVR,Offense,Defense,Abbreviation) %>%
  rename(Opp=Abbreviation)
TE_2015 <- left_join(TE_2015,TEAM.TE,by="Opp")
TE_2015 <- left_join(TE_2015,PassD,by="Opp")
TE_2015 <- left_join(TE_2015,RECD,by="Opp")
TE_2015 <- TE_2015[complete.cases(TE_2015),]
TEModel <- lm(Fantasy.Points ~ Player + OVR + Speed + Agility + Acceleration + Awareness + Catching + Carrying + Jumping +
                Injury + Stamina + Toughness + Trucking + Elusiveness + Ball.Carrier.Vision + Stiff.Arm + Spin.Move +
                Juke.Move + Spectactular.Catch + Catch.In.Traffic + Route.Running + Release + Weight + D.Yds.G + 
                D.P.YDS.G + D.R.YDS.G + D.PTS.G + TEAM.OVR + Offense + Defense + Kick.Return + INT.D + D.TD + 
                D.TD1 + SOLO + AST + SACK + PD + LONG + FF + YDS.A + TD.G + YDS.G + 0, data= TE_2015)
TEMODE <- step(TEModel,direction="both")

######################################################################################################

TOTALO <- rename(TOTALO,Opp = TEAM)
D_2015 <- left_join(D_2015,TOTALO,by="Opp")
DownsD <- rename(DownsD,Team = TEAM)
D_2015 <- left_join(D_2015,DownsD,by="Team")

D_Model <- lm(Fantasy.Points ~ YDS.G + P.YDS.G + R.YDS.G + PTS.G + TOTAL + RUSH.y + 
                PASS.y + PEN + MADE + ATT + PCT + MADE.1 + ATT.1 + PCT.1 + TOTAL.1 +
                YDS.y + 0,data=D_2015)
DMODE <- step(D_Model,direction="both")

######################################################################################################

FanDuel <- read.csv("C:/Users/193344/Desktop/Week5/FanDuel-NFL-2015-10-11-13183-players-list.csv",
                    stringsAsFactors=FALSE)

FDQB <- filter(FanDuel,Position=="QB") %>%
  mutate(Player = paste(First.Name,Last.Name)) %>%
  rename(Opp = Opponent) %>%
  select(Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)

FDQB <- left_join(FDQB,MaddenQB,by="Player")
FDQB <- left_join(FDQB,TOTALD.QB,by="Opp")
FDQB <- left_join(FDQB,PassD,by="Opp")
FDQB <- left_join(FDQB,TEAM.QB,by="Opp")
FDQB <- left_join(FDQB,RECD,by="Opp")
FDQB <- FDQB[complete.cases(FDQB),]

#######################################################################################################

FDRB <- filter(FanDuel,Position=="RB") %>%
  mutate(Player = paste(First.Name,Last.Name)) %>%
  rename(Opp = Opponent) %>%
  select(Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)

FDRB <- left_join(FDRB,MaddenRB,by="Player")
FDRB <- left_join(FDRB,TOTALD.RB,by="Opp")
FDRB <- left_join(FDRB,PassD,by="Opp")
FDRB <- left_join(FDRB,TEAM.RB,by="Opp")
FDRB <- left_join(FDRB,RECD,by="Opp")
FDRB <- FDRB[complete.cases(FDRB),]

#######################################################################################################

FDWR <- filter(FanDuel,Position=="WR") %>%
  mutate(Player = paste(First.Name,Last.Name)) %>%
  rename(Opp = Opponent) %>%
  select(Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)

FDWR <- left_join(FDWR,MaddenWR,by="Player")
FDWR <- left_join(FDWR,TOTALD.WR,by="Opp")
FDWR <- left_join(FDWR,PassD,by="Opp")
FDWR <- left_join(FDWR,TEAM.WR,by="Opp")
FDWR <- left_join(FDWR,RECD,by="Opp")
FDWR <- FDWR[complete.cases(FDWR),]

######################################################################################################

FDTE <- filter(FanDuel,Position=="TE") %>%
  mutate(Player = paste(First.Name,Last.Name)) %>%
  rename(Opp = Opponent) %>%
  select(Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)

FDTE <- left_join(FDTE,MaddenTE,by="Player")
FDTE <- left_join(FDTE,TOTALD.TE,by="Opp")
FDTE <- left_join(FDTE,PassD,by="Opp")
FDTE <- left_join(FDTE,TEAM.TE,by="Opp")
FDTE <- left_join(FDTE,RECD,by="Opp")
FDTE <- FDTE[complete.cases(FDTE),]

######################################################################################################

FDD <- filter(FanDuel,Position=="D") %>%
  mutate(Player = paste(First.Name,Last.Name)) %>%
  rename(Opp = Opponent) %>%
  select(Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)
FDD <- left_join(FDD,TOTALO,by="Opp")
FDD <- left_join(FDD,DownsD,by="Team")

######################################################################################################

QBNames <- QB_2015$Player
RBNames <- RB_2015$Player
WRNames <- WR_2015$Player
TENames <- TE_2015$Player

######################################################################################################

FDQB <- FDQB[FDQB$Player %in% QBNames,]
FDRB <- FDRB[FDRB$Player %in% RBNames,]
FDWR <- FDWR[FDWR$Player %in% WRNames,]
FDTE <- FDTE[FDTE$Player %in% TENames,]

######################################################################################################

QB.PRED <- predict(QBMODE,newdata=FDQB)
RB.PRED <- predict(RBMODE,newdata=FDRB)
WR.PRED <- predict(WRMODE,newdata=FDWR)
TE.PRED <- predict(TEMODE,newdata=FDTE)
D.PRED <- predict(DMODE,newdata=FDD)

######################################################################################################

QBFIN <- select(FDQB,Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)
QBFIN <- cbind(QBFIN,QB.PRED)%>%
  rename(Predicted = QB.PRED)

RBFIN <- select(FDRB,Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)
RBFIN <- cbind(RBFIN,RB.PRED)%>%
  rename(Predicted = RB.PRED)

WRFIN <- select(FDWR,Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)
WRFIN <- cbind(WRFIN,WR.PRED)%>%
  rename(Predicted = WR.PRED)

TEFIN <- select(FDTE,Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)
TEFIN <- cbind(TEFIN,TE.PRED) %>%
  rename(Predicted = TE.PRED)

DFIN <- select(FDD,Player,Position,FPPG,Salary,Team,Opp,Injury.Indicator,Injury.Details)
DFIN <- cbind(DFIN,D.PRED)%>%
  rename(Predicted = D.PRED)

######################################################################################################

Final <- rbind(QBFIN,RBFIN,WRFIN,TEFIN,DFIN)
setwd("C:/Users/Jeremiah Lowhorn/Desktop/Lineups")
file <- paste("Final","_",Sys.Date(),".csv")
write.csv(Final,file)
