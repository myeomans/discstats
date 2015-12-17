#THIS LABELS WHICH SLICE I'M TAKING 
VIEW <- setClass("VIEW", 
                 representation(cut="vector",
                                xr="vector",
                                yr="vector",
                                name="character"))

VIEWS<-list() 
#VIEWS[[1]]<-VIEW(cut=(Play$SLL==1), xr=c(-30,30),  yr=c(-15,75), name="Left Sideline")
#VIEWS[[2]]<-VIEW(cut=(Play$MF==1),  xr=c(-30,30), yr=c(-15,75), name="MidField")
#VIEWS[[3]]<-VIEW(cut=(Play$SLR==1), xr=c(-30,30), yr=c(-15,75), name="Right Sideline")

#VIEWS[[1]]<-VIEW(cut=((Play$SLL==1)&(Play$pickup==1)), xr=c(-30,30),  yr=c(-15,75), name="Left Sideline")
#VIEWS[[2]]<-VIEW(cut=((Play$MF==1)&(Play$pickup==1)),  xr=c(-30,30), yr=c(-15,75), name="MidField")
#VIEWS[[3]]<-VIEW(cut=((Play$SLR==1)&(Play$pickup==1)), xr=c(-30,30), yr=c(-15,75), name="Right Sideline")
#VIEWS[[1]]<-VIEW(cut=(Play$pickup==1), xr=c(-30,30), yr=c(-15,75), name="Pickups")

VIEWS[[1]]<-VIEW(cut=TRUE,  xr=c(-30,30), yr=c(-15,75), name="All")
#VIEWS[[2]]<-VIEW(cut=(Play$RZ==1),  xr=c(-30,30), yr=c(-15,75), name="Red Zone")

source("PLAYERS.R")
source("drawfield.R")
source("drawatlas.R")

COLOURS <-c("blue",      "red",       "darkgreen")
COLOURS2<-c("lightblue", "lightpink", "lightgreen")