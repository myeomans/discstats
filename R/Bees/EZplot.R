###ENDZONE BINS
xline<-c(-20, -10, 0, 10, 25)
yline<-c(-20, -12, -4, 4, 12, 20)

Play$xbin <-cut(x=Play$EZdist,  breaks=xline, labels=FALSE)
Play$ybin <-cut(x=Play$LRdist,  breaks=yline, labels=FALSE)
Play$xbin2<-cut(x=Play$EZdist2, breaks=xline, labels=FALSE)
Play$ybin2<-cut(x=Play$LRdist2, breaks=yline, labels=FALSE)

filter<-((Play$homeOnOffense==1)&!(Play$throwerId==0))
Tgrid<-array(0, c(ncol(xbins), ncol(ybins)))
for (x in 1:ncol(xbins)){
  for (y in 1:ncol(ybins)){
    Tgrid[x,y]<-nrow(Play[which(filter&(Play$xbin==x)&(Play$ybin==y)),])
}}
Cgrid<-array(0, c(ncol(xbins), ncol(ybins)))
for (x in 1:ncol(xbins)){
  for (y in 1:ncol(ybins)){
    Cgrid[x,y]<-nrow(Play[which(filter&(Play$xbin2==x)&(Play$ybin2==y)),])
}}


###ENDZONE GRAPH
library(gplots)
#par(bg="palegreen")
plotCI(x=0, y=-20, pch=13, col="blue", cex=2,#THE BRICK MARK
       xlim=c(-20,20), ylim=c(-20, 25),
       xaxt="n", yaxt="n",
       ylab="Yards from Goal Line",
       xlab="Yards from Center",
       main="END ZONE OFFENSE"
)

axis(side=1, at=yline, lab=yline)
axis(side=2, at=xline, lab=xline)
abline(h=0)

##########MAIN CATCH-THROW PAIRS############
xtic<-(xline[1:(length(xline)-1)]+xline[2:length(xline)])/2
ytic<-(yline[1:(length(yline)-1)]+yline[2:length(yline)])/2

for (i in 1:nrow(Tgrid)){ 
  points(x=ytic, y=rep(xtic[i], length(ytic)), pch=16,
         cex=sqrt(Tgrid[i,]), col="dark blue") 
  #points(x=((1:13)+0.5), y=rep((i+0.5),13), pch=16,
   #      cex=sqrt(4*(Bees[[l]]@ThrOs[,i])), col="red") 
}