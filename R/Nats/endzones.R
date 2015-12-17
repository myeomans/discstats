####################################################
#SIDELINES
####################################################
Play$SL<-1*((Play$LRdist<(-10))|(Play$LRdist>10))
Play$MF<-(1-(Play$SL))

#WHAT SIDELINE WAS IT THROWN FROM?
Play$SLL<-1*(Play$SL&(Play$LRdist<0))
Play$SLR<-1*(Play$SL&(Play$LRdist>0))

Play$swing<-1*((abs(Play$LRspan)>15)&(abs(Play$LRspan)>abs(Play$FWspan)))
####################################################
#RED ZONE ADJUSTMENTS
####################################################
Play$RZ<-1*(Play$EZdist>(-20))
Play$plane<-1*((Play$score==1)|((Play$EZdist2>(-0.1))&((Play$TO==1)|(Play$drop==1))))
####################################################
#RED ZONE ENTRIES
####################################################
#Play$RZstart<-1*((Play$RZ)&(Play$pickup))
#Play$RZhuck<-0
#for (i in 2:nrow(Play)){
#  #PICKUPS AND Ds
#  if((Play[i,]$RZ==1)
#     &(Play[i,]$pickup==0)
#     &(Play[(i-1),]$RZ==0)#NEED TO RE-DEFINE THIS LINE
#     ){                   #TO BE ROBUST TO MULTIPLE RZ ENTRIES
#    Play[i,]$RZstart<-1
#  }
#  #HUCKS INTO THE RZ
#  if((Play[i,]$RZstart==1)
#     &(Play[(i-1),]$EZdist>(-35))){
#    Play[i,]$RZhuck<-1
#  }
#print(i)}