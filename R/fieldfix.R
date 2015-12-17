field.fix<-function(teamdata){
  #COMBINE ALL THROW TURNOVERS
  teamdata$Play$TO      <-(1*(teamdata$Play$throwaway==1)+
                             1*(teamdata$Play$intercept==1)+
                             1*(teamdata$Play$block==1)+
                             1*(teamdata$Play$stall==1))

  #ADJUST FIELD POSITION FOR DIRECTIONS
  HR<-which(xor((teamdata$Play$homeAttackingRight==1), (teamdata$Play$homeOnOffense==0)))

  #HOW FAR FROM THE GOAL LINE WAS THE THROW?
  teamdata$Play$EZdist<-0
  teamdata$Play[ HR,]$EZdist<-(teamdata$Play[ HR,]$lastXPosition-7500)*(70/5000)
  teamdata$Play[-HR,]$EZdist<-(2500-teamdata$Play[-HR,]$lastXPosition)*(70/5000)
  #HOW FAR FROM THE GOAL LINE WAS THE CATCH?
  teamdata$Play$EZdist2<-0
  teamdata$Play[ HR,]$EZdist2<-(teamdata$Play[ HR,]$xPosition-7500)*(70/5000)
  teamdata$Play[-HR,]$EZdist2<-(2500-teamdata$Play[-HR,]$xPosition)*(70/5000)
  #HOW FAR RIGHT FROM CENTER WAS THE THROW?
  teamdata$Play$LRdist<-NA
  teamdata$Play[ HR,]$LRdist<-(teamdata$Play[ HR,]$lastYPosition-1000)*(40/8000)-20
  teamdata$Play[-HR,]$LRdist<-(9000-teamdata$Play[-HR,]$lastYPosition)*(40/8000)-20
  #HOW FAR RIGHT FROM CENTER WAS THE CATCH?
  teamdata$Play$LRdist2<-NA
  teamdata$Play[ HR,]$LRdist2<-(teamdata$Play[ HR,]$yPosition-1000)*(40/8000)-20
  teamdata$Play[-HR,]$LRdist2<-(9000-teamdata$Play[-HR,]$yPosition)*(40/8000)-20
  #WAS THIS THROW MADE FROM THE RED ZONE?
  teamdata$Play$RZ<-1*(teamdata$Play$EZdist>(-20))
  #WAS THIS THROW MADE FROM NEAR THE SIDELINE?
  teamdata$Play$SL<-1*(teamdata$Play$lastYPosition<(1000+(8000/5))|(teamdata$Play$lastYPosition>(1000+(8000*4/5))))

  #CORRECTING THROWS LOCATED OVER THE GOAL LINE
  teamdata$Play[(which(teamdata$Play$EZdist>0)-1),]$EZdist2<-(-0.1)
  teamdata$Play[ which(teamdata$Play$EZdist>0)   ,]$EZdist <-(-0.1)
  teamdata$Play[(which(teamdata$Play$EZdist2>25)),]$EZdist2<-(24.9)

  #CORRECTING THROWS OUTSIDE THE SIDELINE
  teamdata$Play[(which(teamdata$Play$LRdist2>  20 )),]$LRdist2<- ( 19.9)
  teamdata$Play[(which(teamdata$Play$LRdist2<(-20))),]$LRdist2<- (-19.9)
  teamdata$Play[(which(teamdata$Play$LRdist>  20 )  ),]$LRdist <-( 19.9)
  teamdata$Play[(which(teamdata$Play$LRdist<(-20))  ),]$LRdist <-(-19.9)

  return(teamdata)
}

# FIELD DIMENSIONS

field <- list()
field$endzone_left_back <- 440
field$endzone_left_front <- 2500
field$endzone_right_front <- 7500
field$endzone_right_back <- 9560

field$x_length <- field$endzone_right_back - field$endzone_left_back
field$x_mid <- field$endzone_left_back + field$x_length/2
field$x_step <- 20
field$y_step <- 40
field$ob_top <- 1000
field$ob_bottom <- 9000
field$y_length <- field$ob_bottom - field$ob_top
field$y_mid <- field$ob_top + field$y_length/2

