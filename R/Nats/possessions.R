source("compiling.R")

Pfinder <- function (x, y) which((Play$gameId==x)&(Play$playId==y))

###################################################
#BEGINNING OF A POSSESSION
###################################################
Play$pickup<-1*(Play$playId==0)
for(i in unique(Play$gameId)){
  for(j in 1:max(Play[which(Play$gameId==i),]$playId)){
    if((Play[Pfinder(i,j),]$homeOnOffense==1)
       &(Play[Pfinder(i,(j-1)),]$homeOnOffense==0)){          
      Play[Pfinder(i,j),]$pickup<-1 #after every change of possession
    }
    Play[Pfinder(i,j),]$possscore<-sum(Play[Pfinder(i,j),]$score)
print(paste(j, "-", i))}}
###################################################
#POINT- AND POSSESSION-LEVEL CODING
###################################################
#Play<-Play[-3255,] #SO WEIRD...

Play$possscore<-0
Play$Oline<-0
for (i in 1:nrow(Play)){
  Play[i,]$possscore<-sum(Play[which((Play$gameId==Play[i,]$gameId)
                                    &(Play$possessionId==Play[i,]$possessionId)
                          ),]$score)
  POINT<-((Play$gameId==Play[i,]$gameId)&(Play$pointId==Play[i,]$pointId))  
  Play[i,]$Oline<-Play[which(POINT
                            &Play$playId==min(Play[which(POINT),]$playId)),]$homeOnOffense
print(i)}