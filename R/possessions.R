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

#DID THIS POSSESSION SCORE?
Play$possscore<-0
#WAS THIS AN O-LINE POSSESSION?
Play$Oline<-0
for (i in 1:nrow(Play)){
  Play[i,]$possscore<-sum(Play[which((Play$gameId==Play[i,]$gameId)
                                     &(Play$possessionId==Play[i,]$possessionId)
  ),]$score)
  POINT<-((Play$gameId==Play[i,]$gameId)&(Play$pointId==Play[i,]$pointId))
  Play[i,]$Oline<-Play[which(POINT&Play$playId==min(Play[which(POINT),]$playId)),]$homeOnOffense
  print(i)}

Pfinder <- function (x, y) which((Play$gameId==x)&(Play$playId==y))

#FIND THROWS OFF OF PICKUPS AND Ds
Play$pickup<-0
Play[which(Play$playId==0),]$pickup<-1
for(i in unique(Play$gameId)){
  for(j in 1:max(Play[which(Play$gameId==i),]$playId)){
    if((Play[Pfinder(i,j),]$homeOnOffense==1)
       &(Play[Pfinder(i,(j-1)),]$homeOnOffense==0)){
      Play[Pfinder(i,j),]$pickup<-1 #after every change of possession
    }
    Play[Pfinder(i,j),]$possscore<-sum(Play[Pfinder(i,j),]$score)
    print(paste(j, "-", i))}}
