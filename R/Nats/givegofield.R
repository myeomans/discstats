TD<-c(1372946253, 1372946801)
BW<- c(1378345407, 1378346043)
TMDA<-c(1381697882, 1381698237)

#HIPAIRS<-table(Play[which(Play$GG==1),]$throwrec)[which(table(Play[which(Play$GG==1),]$throwrec)>20)]
#common<-paste(names(HIPAIRS), collapse=" ")

GGselect<-which((Play$throwrec==paste(TMDA, collapse=" "))&(Play$GG==1))
GGname<-"McKnight Alexander"
#GGname<- "Thorne DiGioralmo"
#GGname<- "Bob Walden"
#COLS<-c("dodgerblue", "firebrick")
#COLS<-c("forestgreen","tan4")
COLS<-c("black","firebrick")









png(paste("plots/", GGname, " THROW.png", sep=""), width=800, height=1500)
par(mfrow=c(1,1))
#THIS IS AN INDEX OF WHICH ROWS CORRESPOND TO PLAYER P's THROWS
THROWS<-GGselect
#CCODE<-1+ #COLOR CODES: 1=COMPLETION, 2=TO, 3=GOAL
#    1*(Play[THROWS,]$TO)+
#    2*(Play[THROWS,]$score)
CCODE<-1+(Play[THROWS,]$throwerId==BW[2])
################################################
drawfield("throw", GGname, "Give-Go") #BLANK BACKGROUND W/ TITLES
points(EZdist2~LRdist2,     
       data=Play[THROWS,],
       col=COLS[(3-CCODE)],
       cex=3, lwd=2, xpd=TRUE)
points(EZdist~LRdist,   
       data=Play[THROWS,],
       col=COLS[CCODE],
       cex=2, lwd=2, pch=20, xpd=TRUE)
segments(x0=Play[THROWS,]$LRdist,
         x1=Play[THROWS,]$LRdist2,
           y0=Play[THROWS,]$EZdist,
           y1=Play[THROWS,]$EZdist2,
           col=COLS[CCODE],
           lwd=2, lty=2, xpd=TRUE)
points(EZdist~LRdist,   
     data=Play[THROWS-1,],
     col=COLS[(3-CCODE)],
     cex=2, lwd=2, pch=20, xpd=TRUE)
segments(x0=Play[THROWS-1,]$LRdist,
       x1=Play[THROWS-1,]$LRdist2,
       y0=Play[THROWS-1,]$EZdist,
       y1=Play[THROWS-1,]$EZdist2,
       col=COLS[(3-CCODE)],
       lwd=2, lty=2, xpd=TRUE)
dev.off()


