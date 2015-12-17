drawfield<-function(type, name, view){

#SETS THE MARGINS
par(mar=c(5,5,1,1))

#DRAWS AN EMPTY PLOT
plot(0,
     xaxt="n", xlim=c(-19,19), xlab="Yards Wide",
     yaxt="n", ylim=c(-90,20), ylab="Yards From End Zone",
     col="white", cex.lab=2.5)

#TITLES
text(x=-20,  y=22, cex=2.5, pos=4, name)
text(x= 20,  y=22, cex=2.5, pos=2, paste("Thrown from ", view))

#AXES - NOTE THAT xaxt & yaxt ABOVE TURN OFF THE AUTOMATIC AXES, SO WE CAN DRAW THEM HERE
axis(side=1, at=c(-2:2)*10, label=abs(c(-2:2)*10), cex.axis=2)
axis(side=2, at=c(-9:2)*10, label=abs(c(-9:2)*10), cex.axis=2)

#END ZONE LINES
abline(h=  0, col="black")
abline(h=-70, col="black")
#abline(v=0, col="gray")
points(x=c(0,0), y=c(-50, -20),
       pch=19, cex=2)

#segments(x0= -8,  x1= -8, y0= -70, y1= -15, col="deepskyblue", lty=3)
#segments(x0=  8,  x1=  8, y0= -70, y1= -15, col="deepskyblue", lty=3)
abline(h=  -15, col="firebrick", lty=2)

if (type=="throw"){LL<-c("Completion", "Turnover",  "Assist")}
if (type=="catch"){LL<-c("Catch",      "Drop",      "Goal")}

#COLORS AND LEGEND
#legend (x=-20, y=-80, cex=2.8, legend=LL, xpd=TRUE,
#        pch=1, col=COLOURS)
}