colfunc <- colorRampPalette(c("dark green", "yellow", "firebrick3"))
smear<-colfunc(50)

source('C:/Users/Mike/Google Drive/Frisbee/Nats/plotprep.R')
source('C:/Users/Mike/Google Drive/Frisbee/Nats/drawatlas.R')

CUTS<-list() 
CUTS[[1]]<-VIEW(cut=T,                              xr=c(-30,30),  yr=c(-15,75), name="ALL")
CUTS[[2]]<-VIEW(cut=(Play$SLL==1),                   xr=c(-30,30),  yr=c(-15,75), name="L sideline")
CUTS[[3]]<-VIEW(cut=(Play$SLR==1),                   xr=c(-30,30),  yr=c(-15,75), name="R sideline")
CUTS[[4]]<-VIEW(cut=(Play$SL==0),                   xr=c(-30,30),  yr=c(-15,75), name="no sideline")
#CUTS[[1]]<-VIEW(cut=(Play$RZ==1),                   xr=c(-30,30),  yr=c(-15,75), name="Red Zone (All Throws)")
#CUTS[[2]]<-VIEW(cut=((Play$RZ==1)&(Play$plane==1)), xr=c(-30,30),  yr=c(-15,75), name="Red Zone (Assist Attempts)")
#CUTS[[3]]<-VIEW(cut=((Play$RZ==1)&(Play$plane==0)), xr=c(-30,30),  yr=c(-15,75), name="Red Zone (No Assist Attempts)")
#CUTS[[4]]<-VIEW(cut=(Play$RZ==0),                   xr=c(-30,30),  yr=c(-15,75), name="Rest of Field (All Throws)")
#CUTS[[5]]<-VIEW(cut=((Play$RZ==0)&(Play$plane==1)), xr=c(-30,30),  yr=c(-15,75), name="Rest of Field (Assist Attempts)")
#CUTS[[6]]<-VIEW(cut=((Play$RZ==0)&(Play$plane==0)), xr=c(-30,30),  yr=c(-15,75), name="Rest of Field (No Assist Attempts)")  
  
#for (P in 1:length(PLAYERS)){
for (C in 1:length(CUTS)){
  png(paste("plots/PASSATLAS ", CUTS[[C]]@name, ".png", sep=""), width=1000, height=1400)
  #par(mfrow=c(1,length(CUTS)))
  par(mfrow=c(1,1))
  ################################################
  drawatlas("throw", " ", CUTS[[C]]@name)

  #THROWS<-Play[which(CUTS[[C]]@cut),]  
  THROWS<-Play
  for (x in 1:12){
    for (y in 1:18){
      xpt<- 5*(x-6.5)
      ypt<- 5*(y-9.5)
  
  
  #for (x in 1:6){
  #  for (y in 1:9){
  #    xpt<- 10*(x-3.5)
  #    ypt<- 10*(y-2.5)
      
      #MANHATTAN DISTANCE
      TILE<-which((abs(THROWS$LRspan-xpt)<=2.5)
                 &(abs(THROWS$FWspan-ypt)<=2.5)
                 &CUTS[[C]]@cut)
      #PYTHAGOREAN DISTANCE
      #which(sqrt(((THROWS$LRspan-xpt)**2)
      #          +((THROWS$FWspan-ypt)**2))<=5)
      
      TOP<-mean(THROWS[TILE,]$TO)
      if(is.na(TOP))  {colour<-"white"}
      else if(TOP>0.5){colour<-"firebrick3"}
      else            {colour<-smear[round(TOP*100, 0)]}
      
      #SQUARE TILES
      WGT<-(round(sqrt(nrow(THROWS[TILE,])), 2))
      if(WGT<3){colour<-"white"}
      text((100*round(TOP, 2)), x=xpt, y=ypt, cex=4, col=colour)
      #points(x=xpt, y=ypt,
      #       col=colour,
      #       cex=WGT, lwd=2, xpd=TRUE) 
      
      #LEGEND
      points(x=-25, y=75, 
             cex=sqrt(10),
             lwd=2, xpd=TRUE)
      text("10 throws", x=-25, y=71, cex=3)
    }
  }
dev.off()
THROWS<-NULL
print(C)} 
#  print(P)}