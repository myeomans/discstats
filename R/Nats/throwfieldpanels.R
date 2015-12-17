source('C:/Users/Mike/Google Drive/Frisbee/Nats/panelprep.R')
source('C:/Users/Mike/Google Drive/Frisbee/Nats/drawfield.R')

for (P in 1:length(PLAYERS)){
  png(paste("plots/", PLAYERS[[P]]@name, " THROW.png", sep=""), width=2400, height=1500)
  par(mfrow=c(1,length(VIEWS)))
  for (V in 1:length(VIEWS)){
    #THIS IS AN INDEX OF WHICH ROWS CORRESPOND TO PLAYER P's THROWS
    THROWS<-which((Play$throwerId==PLAYERS[[P]]@pID)&VIEWS[[V]]@cut)
    CCODE<-1+ #COLOR CODES: 1=COMPLETION, 2=TO, 3=GOAL
      1*(Play[THROWS,]$TO)+
      2*(Play[THROWS,]$score)
    ################################################
    drawfield("throw", PLAYERS[[P]]@name, VIEWS[[V]]@name) #BLANK BACKGROUND W/ TITLES
    points(EZdist2~LRdist2,   
           data=Play[THROWS,],
           col=COLOURS[CCODE],
           cex=3, lwd=2, xpd=TRUE)
    points(EZdist~LRdist,   
           data=Play[THROWS,],
           col=COLOURS[CCODE],
           cex=2, lwd=2, pch=20, xpd=TRUE)
    segments(x0=Play[THROWS,]$LRdist,
             x1=Play[THROWS,]$LRdist2,
             y0=Play[THROWS,]$EZdist,
             y1=Play[THROWS,]$EZdist2,
             col=COLOURS2[CCODE],
             lwd=1, lty=2, xpd=TRUE)
  }
  dev.off()
print(P)}