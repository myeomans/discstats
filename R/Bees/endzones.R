Play$plane<-1*((Play$score==1)|((Play$EZdist2>(-0.1))&((Play$TO==1)|(Play$drop==1))))
Play$swing<-1*((abs(Play$LRdist-Play$LRdist2)>15)
              &((Play$LRdist2-Play$LRdist)*Play$LRdist<0))
L<-which(Play$plane==0&Play$homeOnOffense==1&Play$rushingDistance<25)
summary(Play[L,]$TO+Play[L,]$drop)
Play$RZstart<-1*((Play$RZ)&(Play$pickup))
Play$RZhuck<-0
for (i in 2:nrow(Play))
{
  #PICKUPS AND Ds
  if((Play[i,]$RZ==1)
     &(Play[i,]$pickup==0)
     &(Play[(i-1),]$RZ==0)#NEED TO RE-DEFINE THIS LINE
     ){                   #TO BE ROBUST TO MULTIPLE RZ ENTRIES
    Play[i,]$RZstart<-1
  }
  #HUCKS INTO THE RZ
  if((Play[i,]$RZstart==1)
     &(Play[(i-1),]$EZdist>(-35))){
    Play[i,]$RZhuck<-1
  }
print(i)}

RZTOs <- array(NA, c(nrow(Player), 4))
for (i in 1:nrow(Player)){
  PID<-Player[i,]$id
  RZTOs[i, 1]<-Player[i,]$name
  RZTOs[i, 2]<-sum(Play[which(Play$RZ==1&Play$throwerId==PID),]$TO)+sum(Play[which(Play$RZ==1&Play$receivererId==PID),]$drops)
  RZTOs[i, 3]<-length(Play[which(Play$RZ==1&Play$throwerId==PID),]$TO)+length(Play[which(Play$RZ==1&Play$receiverId==PID),]$score)
  RZTOs[i, 4]<-as.numeric(RZTOs[i,2])/(as.numeric(RZTOs[i,2])+as.numeric(RZTOs[i,3]))
print(i)}