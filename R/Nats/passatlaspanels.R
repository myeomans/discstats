source('panelprep.R')
source('drawatlas.R')

for (P in 1:length(PLAYERS)){
  png(paste("plots/", PLAYERS[[P]]@name, " PASSES.png", sep=""), width=3000, height=1400)
  par(mfrow=c(1,length(VIEWS)))
  for (V in 1:length(VIEWS)){
    THROWS<-which((Play$throwerId==PLAYERS[[P]]@pID)&VIEWS[[V]]@cut)
    CCODE<-(1+ #COLOR CODES: 1=COMPLETION, 2=TO, 3=GOAL
              1*(Play[THROWS,]$TO)+
              2*(Play[THROWS,]$score))
    ################################################
    drawatlas("throw", PLAYERS[[P]]@name, VIEWS[[V]]@name)
    points(FWspan~LRspan,    
           data=Play[THROWS,],
           col=COLOURS[CCODE],
           cex=3, lwd=2, xpd=TRUE) #This assigns colors to every throw based on CCODE
    }
  dev.off()
print(P)}