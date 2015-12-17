source('C:/Users/Mike/Google Drive/Frisbee/Nats/panelprep.R')
source('C:/Users/Mike/Google Drive/Frisbee/Nats/drawatlas.R')

colfunc <- colorRampPalette(c("dark green", "yellow", "firebrick3"))
smear<-colfunc(50)


CUTS<-list() 
CUTS[[1]]<-VIEW(cut=(Play$RZ==1),                   xr=c(-30,30),  yr=c(-15,75), name="Red Zone (All Throws)")
CUTS[[2]]<-VIEW(cut=((Play$RZ==1)&(Play$plane==1)), xr=c(-30,30),  yr=c(-15,75), name="Red Zone (Assist Attempts)")
CUTS[[3]]<-VIEW(cut=((Play$RZ==1)&(Play$plane==0)), xr=c(-30,30),  yr=c(-15,75), name="Red Zone (No Assist Attempts)")
CUTS[[4]]<-VIEW(cut=(Play$RZ==0),                   xr=c(-30,30),  yr=c(-15,75), name="Rest of Field (All Throws)")
CUTS[[5]]<-VIEW(cut=((Play$RZ==0)&(Play$plane==1)), xr=c(-30,30),  yr=c(-15,75), name="Whole Field (Assist Attempts)")
CUTS[[6]]<-VIEW(cut=((Play$RZ==0)&(Play$plane==0)), xr=c(-30,30),  yr=c(-15,75), name="Whole Field (No Assist Attempts)")  


#for (C in 1:length(CUTS)){
for (C in 1){
  png(paste("plots/", CUTS[[C]]@name, " PASSES.png", sep=""), width=3000, height=1400)
  par(mfrow=c(1,length(VIEWS)))
  for (V in 1:length(VIEWS)){
################################################
    drawatlas("throw", CUTS[[C]]@name, VIEWS[[V]]@name)
    THROWS<-Play[which((CUTS[[C]]@cut)&(VIEWS[[V]]@cut)),]
  #for (x in 1:12){
  #for (y in 1:18){
  #  xpt<- 5*(x-6.5)
  #  ypt<- 5*(y-9.5)    
for (x in 1:6){
  for (y in 1:9){
    xpt<- 10*(x-3.5)
    ypt<- 10*(y-2.5)
      
      #MANHATTAN DISTANCE
      TILE<-which((abs(THROWS$LRspan-xpt)<=2.5)
                  &(abs(THROWS$FWspan-ypt)<=2.5))
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
      points(x=xpt, y=ypt,
             col=colour,
             cex=WGT, lwd=2, xpd=TRUE, add=TRUE) 
      
      #LEGEND
      points(x=-10, y=75, 
             cex=sqrt(10),
             lwd=2, xpd=TRUE, add=TRUE)
      text("10 throws", x=-10, y=71, cex=3)
    }
  }
    THROWS<-NULL}
  dev.off()
print(C)}
