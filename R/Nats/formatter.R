Play<-Play[order(Play$gameId, 
                 Play$pointId, 
                 Play$possessionId, 
                 Play$playId),]

#Play[which(Play$team=="machine"),]$team<-" Machine"
####################################################
#COMBINE ALL THROW TURNOVERS
Play$TO      <-(1*(Play$throwaway==1)+ 
                1*(Play$intercept==1)+
                1*(Play$block==1)+
               1*(Play$stall==1))

####################################################
#ADJUST FIELD POSITION FOR DIRECTIONS
HR<-which(xor((Play$homeAttackingRight==1), (Play$homeOnOffense==0)))
#HOW FAR FROM THE GOAL LINE WAS THE THROW?
Play$EZdist<-0
Play[ HR,]$EZdist<-(Play[ HR,]$lastXPosition-7500)*(70/5000) 
Play[-HR,]$EZdist<-(2500-Play[-HR,]$lastXPosition)*(70/5000) 
#HOW FAR FROM THE GOAL LINE WAS THE CATCH?
Play$EZdist2<-0
Play[ HR,]$EZdist2<-(Play[ HR,]$xPosition-7500)*(70/5000) 
Play[-HR,]$EZdist2<-(2500-Play[-HR,]$xPosition)*(70/5000) 
#HOW FAR RIGHT FROM CENTER WAS THE THROW?
Play$LRdist<-NA
Play[ HR,]$LRdist<-(Play[ HR,]$lastYPosition-1000)*(40/8000)-20
Play[-HR,]$LRdist<-(9000-Play[-HR,]$lastYPosition)*(40/8000)-20       
#HOW FAR RIGHT FROM CENTER WAS THE CATCH?
Play$LRdist2<-NA
Play[ HR,]$LRdist2<-(Play[ HR,]$yPosition-1000)*(40/8000)-20
Play[-HR,]$LRdist2<-(9000-Play[-HR,]$yPosition)*(40/8000)-20

####################################################
#CORRECTING THROWS LOCATED OVER THE GOAL LINE
Play[(which(Play$EZdist>0)-1),]$EZdist2<-(-0.1)
Play[ which(Play$EZdist>0)   ,]$EZdist <-(-0.1)
Play[(which(Play$EZdist2>25)),]$EZdist2<-(24.9)

#CORRECTING THROWS OUTSIDE THE SIDELINE
Play[(which(Play$LRdist2>  20 )),]$LRdist2<- ( 19.9)
Play[(which(Play$LRdist2<(-20))),]$LRdist2<- (-19.9)
Play[(which(Play$LRdist>  20 )  ),]$LRdist <-( 19.9)
Play[(which(Play$LRdist<(-20))  ),]$LRdist <-(-19.9)
####################################################

#DOWNFIELD DISTANCE VECTOR - UP/DOWN IN PASS ATLAS
Play$FWspan<-Play$rushingDistance

#LEFT-RIGHT DISTANCE VECTOR - L/R IN PASS ATLAS
Play$LRspan<-Play$LRdist2-Play$LRdist