
#POSSESSIONS PER POINT
Line$Opos<-NA
Line$Dpos<-NA
for (i in 1:(nrow(Line))){  
  ID<-((Possess$gameId==Line[i,]$gameId)&(Possess$pointId==Line[i,]$pointId))
  Line[i,]$Opos<-nrow(Possess[which(ID&(Possess$homePossession==1)),])
  Line[i,]$Dpos<-nrow(Possess[which(ID&(Possess$homePossession==0)),])  
  print(i)}
#POSSESSIONS PER PLAYER
Player$Opos<-NA
Player$Dpos<-NA
for (i in 1:(nrow(Player))){  
  PT<-Line[which(Line$playerId==Player[i,]$id),]
  Player[i,]$Opos<-sum(PT$Opos)
  Player[i,]$Dpos<-sum(PT$Dpos)
  PT<-NULL
  print(i)}
