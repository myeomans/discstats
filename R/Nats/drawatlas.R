drawatlas<-function(type, name, view){
  xrange=c(-30,30)
  if (view=="Left Sideline") {xrange=c(-20,40)}
  #if (view=="MidField")      {xrange=c(-30,30)}
  if (view=="Right Sideline"){xrange=c(-40,20)}
    
  #xrange=c(-30,30)
  yrange=c(-15,75)
  #SETS THE MARGINS
  par(mar=c(5,5,1,1))
  
  #DRAWS AN EMPTY PLOT
  plot(0,
       xaxt="n", xlim=xrange, xlab="Yards Wide",
       yaxt="n", ylim=yrange, ylab="Yards Downfield",
       col="white", cex.lab=2.5)
  
  text(x=xrange[1], y=75, cex=3, pos=4, name)
  text(x=xrange[2], y=75, cex=3, pos=2, view)
  
  #AXES - NOTE THAT xaxt & yaxt ABOVE TURN OFF THE AUTOMATIC AXES, SO WE CAN DRAW THEM HERE
  axis(side=1, at=c(-4:4)*10, label=abs(c(-4:4)*10), cex.axis=2)
  axis(side=2, at=c(-2:8)*10, label=    c(-2:8)*10 , cex.axis=2)
  
  #LINES AT 0 YARDS L/R AND 0 YARDS DOWNFIELD
  abline(h=0, col="gray")
  abline(v=0, col="gray")
  
  #THESE ARE THE DIAGONAL LINE SEGMENTS - YOU GIVE A STARTING x/y AND AN ENDING x/y
  segments(x0= 2*sqrt(10+2), x1= 60,
           y0= 2*sqrt(10+2), y1= 60,
           col="darkgray")
  segments(x0=-2*sqrt(10+2), x1=-60,
           y0=-2*sqrt(10+2), y1=-60,
           col="darkgray")
  segments(x0=-2*sqrt(10+2), x1=-60,
           y0= 2*sqrt(10+2), y1= 60,
           col="darkgray")
  segments(x0= 2*sqrt(10+2), x1= 60,
           y0=-2*sqrt(10+2), y1=-60,
           col="darkgray")
  
  library("shape")
  
  #THIS IS THE RESET CIRCLE
  plotellipse(rx=10, ry=10, mid=c(0,0), lcol="darkgray", lwd=1)
  
  #THESE ARE THE CIRCULAR DISTANCE ARCS, YOU TELL THE FUNCTION WHAT SWEEP OF THE CIRCLE YOU WANT
  plotellipse(rx=25, ry=25, mid=c(0,0), lcol="darkgray", lwd=1,
              from=(pi/4), to=(3*pi/4))
  plotellipse(rx=45, ry=45, mid=c(0,0), lcol="darkgray", lwd=1,
              from=(pi/4), to=(3*pi/4))
  
  if (type=="throw"){LL<-c("Completion", "Turnover",  "Assist")}
  if (type=="catch"){LL<-c("Catch",      "Drop",      "Goal")}
  
  #LEGEND
  #legend (x=-30, y=70, cex=3, legend=LL, xpd=TRUE,
   #       pch=1, col=COLOURS)
}

png(paste("plots/passgrid.png", sep=""), width=1000, height=1400)
drawatlas("throw", " ", " ")
abline(v=((-3:3)*10))
abline(h=((-1:7)*10))
dev.off()