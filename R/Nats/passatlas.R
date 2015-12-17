for (P in 1:length(PLAYERS)){
  for (V in 1:length(VIEWS)){
    png(paste("plots/", PLAYERS[[P]]@name, " PASSES ", VIEWS[[V]]@name, ".png", sep=""), width=1000, height=1400)
    #par(mfrow=c(1,length(VIEWS)))
    par(mfrow=c(1,1))
    THROWS<-which((Play$throwerId==PLAYERS[[P]]@pID)&VIEWS[[V]]@cut)
    CCODE<-(1+ #COLOR CODES: 1=COMPLETION, 2=TO, 3=GOAL
       1*(Play[THROWS,]$TO)+
       2*(Play[THROWS,]$score==1))
    ################################################
    drawatlas("throw", PLAYERS[[P]]@name, VIEWS[[V]]@name)
    points(FWspan~LRspan,    
           data=Play[THROWS,],
           col=COLOURS[CCODE],
           cex=2, lwd=2, xpd=TRUE) #This assigns colors to every throw based on CCODE
  dev.off()} 
print(P)}