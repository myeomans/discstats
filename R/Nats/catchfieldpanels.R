source('plotprep.R')
source('drawfield.R')

for (P in 1:length(PLAYERS)){
  png(paste("plots/", PLAYERS[[P]]@name, " CATCH.png", sep=""), width=2400, height=1500)
  par(mfrow=c(1,length(VIEWS)))
  for (V in 1:length(VIEWS)){
    
    CATCHES<-which((Play$recieverId==PLAYERS[[P]]@pID)&VIEWS[[V]]@cut)
    CCODE<-1+ #COLOR CODES: 1=COMPLETION, 2=TO, 3=GOAL
      1*(Play[CATCHES,]$drop)+
      2*(Play[CATCHES,]$score)
    ################################################
    drawfield("catch", PLAYERS[[P]]@name, VIEWS[[V]]@name) #BLANK BACKGROUND W/ TITLES
    points(EZdist2~LRdist2,   
           data=Play[CATCHES,],
           col=COLOURS[CCODE],
           cex=3, lwd=2, xpd=TRUE)
    segments(x0=Play[CATCHES,]$LRdist,
             x1=Play[CATCHES,]$LRdist2,
             y0=Play[CATCHES,]$EZdist,
             y1=Play[CATCHES,]$EZdist2,
             col=COLOURS2[CCODE],
             lwd=1, lty=2, xpd=TRUE)
  }
  dev.off()
print(P)}