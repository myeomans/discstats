newgames<-paste(folder, "data", sep="")

Play    <- read.csv(paste(FOLDERS[f], "/eliteMens2013newPlays.csv", sep=""))

#NEED PLAYERS ON THE LINE!!!!!!

FOLDERS <- list.files(path=newgames)
PLAYS  <- FOLDERS[grep("newPlays.csv",           FOLDERS)]
POSSESS<- FOLDERS[grep("newPossessions.csv",     FOLDERS)]
LINES  <- FOLDERS[grep("newPlayersOnTheLine.csv",FOLDERS)]
GAMES  <- FOLDERS[grep("newGames.csv",           FOLDERS)]
PLAYERS<- FOLDERS[grep("newPlayers.csv",         FOLDERS)]

Play    <- read.csv(PLAYS  [1])
Possess <- read.csv(POSSESS[1])
Line    <- read.csv(LINES  [1])
Game    <- read.csv(GAMES  [1])
Player  <- read.csv(PLAYERS[1])


for (f in 2:length(PLAYS)){
  Playadd    <- read.csv(PLAYS  [f])
  Possessadd <- read.csv(POSSESS[f])
  Lineadd    <- read.csv(LINES  [f])
  Gameadd    <- read.csv(GAMES  [f])
  Playeradd  <- read.csv(PLAYERS[f])
  if(nrow(Playadd)>1) Play   <-rbind(Play, Playadd)
  if(nrow(Possadd)>1) Possess<-rbind(Possess, Possadd)
  if(nrow(Lineadd)>1) Line   <-rbind(Line, Lineadd)
  if(nrow(Gameadd)>0) Game   <-rbind(Game, Gameadd)
print(i)}

#CLEAN UP THE MESS
Possadd  <-NULL
Playadd  <-NULL
Lineadd  <-NULL
Gameadd  <-NULL

#MIGHT NEED TO TWEAK FOR NEW DATA
Play$X   <-NULL
Possess$X<-NULL
Line$X   <-NULL
Game$X   <-NULL

#sort by gameID
Play<-Play[with(Play, order(Play$gameId)),]
Play$game<-NA
#rename games IN ORDER
for (i in unique(Play$gameId)){
  Play[which(Play$gameId==i),]$game<-which(unique(Play$gameId)==i)
}