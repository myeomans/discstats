
library(gplots)

bee <- setClass("bee", 
                representation(pID="vector",
                               name="character",
                               colour="character"
                ))

Bees <- list()

namer <- "O Handlers"
#Bees[[1]]<-bee(pID=1372268340, name="Weisbrock", colour="gold")
#Bees[[2]]<-bee(pID=1372267594, name="Fergus", colour="dark green")
#Bees[[3]]<-bee(pID=1372268078, name="Marty", colour="blue")
#Bees[[4]]<-bee(pID=1372267741, name="Bruns", colour="orange")

#namer <- "O Cutters"
#Bees[[1]]<-bee(pID=1371016236, name="Joe", colour="red")
#Bees[[2]]<-bee(pID=1372268185, name="Roush", colour="black")
#Bees[[3]]<-bee(pID=1372268238, name="MattWest", colour="brown")
#Bees[[4]]<-bee(pID=1371016154, name="Yeomans", colour="blue")

#namer <- "D Cutters"
#Bees[[1]]<-bee(pID=1372267864, name="Casey", colour="blue")
#Bees[[2]]<-bee(pID=1372268198, name="Uber", colour="gold")
#Bees[[3]]<-bee(pID=1372267759, name="JimmyR", colour="red")
#Bees[[4]]<-bee(pID=1371016266, name="Grant", colour="green")

#namer <- "D Handlers"
#Bees[[1]]<-bee(pID=1371016203, name="Zubair", colour="orange")
#Bees[[2]]<-bee(pID=1371016317, name="Kurt", colour="blue")
#Bees[[3]]<-bee(pID=1371016180, name="Shane", colour="maroon")
#Bees[[4]]<-bee(pID=1372267990, name="BK", colour="green")

bins <- c(((-3):5)*5, ((4:7)-0.5)*10)
ticks <-bins+2.5+2.5*(bins>20)+2.5*(bins>60)

lines<-list()
fits<-list()
for (l in 1:(length(Bees)+1))
{
  l<-1
  filter<-TRUE
  if (l>1){
    filter<-Plays$X.Thrower.id==Bees[[(l-1)]]@pID
  }
  buckets <- array (0, c(5,length(bins)))
  for (i in 1:length(bins))
  {
    top<- 100
    bot<- bins[i]
    if (i<length(bins)){
      top<- bins[i+1]
    }
    buckets[1,i]<-nrow(Plays[which(filter&(Plays$X.Distance>bot)&(Plays$X.Distance<=(top))&(Plays$TO==0)),])
    buckets[2,i]<-nrow(Plays[which(filter&(Plays$X.Distance>bot)&(Plays$X.Distance<=(top))&(Plays$TO==1)),])
    buckets[3,i]<-buckets[1,i]/(buckets[1,i]+buckets[2,i])
    buckets[4,i]<-(buckets[3,i]*(1-buckets[3,i]))/sqrt((buckets[1,i]+buckets[2,i]))
    buckets[5,i]<-(buckets[1,i]+buckets[2,i])
    #if ()
  }
  lines[[l]]<-buckets[3:5,]
}

#png(paste(namer, ".png"), width=1500, height=800)
plotCI(y = lines[[1]][1,], x=ticks, uiw = lines[[1]][2,], 
       lty = 1, cex.lab=1.3, col="dark blue", type="o", 
       sfrac=.005, pch=16, gap = 0, 
       cex=1.5,
       xlim =c(-20,80), yaxt="n", xaxt="n", ylim = c(0,1), cex.main=2.5,
       ylab="Completion Percentage", xlab="Throw Distance", 
       main = paste(namer, " Throws"))

L1<-c("ALL")
L2<-c("dark blue")
for (i in 2:(length(lines)))
{
  plotCI(y = lines[[i]][1,], x=ticks, uiw = lines[[i]][2,], 
       lty = 1, , cex=sqrt(lines[[i]][3,]),  col=Bees[[(i-1)]]@colour,   
       type="o", sfrac=.005, pch=16, gap = 0, add=TRUE)
  L1<-c(L1, Bees[[(i-1)]]@name)
  L2<-c(L2, Bees[[(i-1)]]@colour)
}
axis(side=1, at=c((-4:16)*5), labels=c((-4:16)*5), cex.axis=1.7)
axis(side=2, at=((0:10)/10), labels=c((0:10)/10), cex.axis=1.2)
abline(h=0.5, col="grey")
abline(v=0, col="grey")
legend (x=-20,y=.6,legend=L1, pt.bg=L2,
        bg="white",
        xpd=TRUE, pch=c(21,21),  cex=3)
#text(6.3, 3.9, "T Values", cex=1.8, adj=0, xpd=TRUE)
dev.off()
  
  
#Plays$comp<-1*(Plays$X.Throwaway==" false"&Plays$X.Drop==" false")
#dist<-glm(Plays$comp~Plays$X.Distance, family=binomial)