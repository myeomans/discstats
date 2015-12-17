library(gplots)

bee <- setClass("bee", 
                representation(pID="vector",
                               name="character",
                               colour="character",
                               goals="array",
                               drops="array",
                               ThrCs="array",
                               ThrOs="array",
                               pupCs="array",
                               pupOs="array"
                ))
bins <- c(((-3):5)*5, ((4:7)-0.5)*10)
ticks <-bins+2.5+2.5*(bins>20)+2.5*(bins>60)

Bees <- list()

for (i in 1:nrow(Players))
{
  Bees[[i]] <-bee(pID=Players[i,]$Player.Id, 
                  name=as.character(Players[i,]$Player.Name),
                  ThrCs=array (0, c(length(bins),length(bins))),
                  ThrOs=array (0, c(length(bins),length(bins))),
                  goals=array (0, c(1,length(bins))),
                  drops=array (0, c(1,length(bins))),
                  pupCs=array (0, c(1,length(bins))),
                  pupOs=array (0, c(1,length(bins)))
  )
}
Bees[[(nrow(Players)+1)]]<-bee(pID=0, 
                               name="EveryBee",
                               ThrCs=array (0, c(length(bins),length(bins))),
                               ThrOs=array (0, c(length(bins),length(bins))),
                               goals=array (0, c(1,length(bins))),
                               drops=array (0, c(1,length(bins))),
                               pupCs=array (0, c(1,length(bins))),
                               pupOs=array (0, c(1,length(bins))))

for (l in 1:length(Bees))
{
  filterc<-rep(TRUE, nrow(Plays))
  if (Bees[[(l)]]@pID>0){ ###pID=0 is the whole team
    filterc<-Plays$X.Reciever.Id==Bees[[(l)]]@pID
  } 
  for (k in 1:nrow(Plays)){
    if (filterc[k]){ ##IF OUR BEE MADE THIS CATCH###
      print(k)
      for (i in 1:length(bins)){  
        topc<- 100 ###FIND THE CATCH BIN###
        botc<- bins[i]
        if (i<length(bins)){topc<- bins[i+1]}
        if((Plays[k,]$X.Distance>botc)
           &(Plays[k,]$X.Distance<=(topc))){
          catchbin<-i  
        }
      }####################################
      if (Plays[k,]$Score==1){     ####SCORE###
        Bees[[l]]@goals[1,catchbin]<-Bees[[l]]@goals[1,catchbin]+1
      }
      else if (Plays[k,]$Drop==1){ ####DROP####
                                   Bees[[l]]@drops[1,catchbin]<-Bees[[l]]@drops[1,catchbin]+1
      }
      else
      {                            ####THROW###
        for (j in 1:length(bins)){###BINS###
          topt<- 100 
          bott<- bins[j]
          if (j<length(bins)){
            topt<- bins[j+1]
          }
          if ((Plays[k+1,]$X.Distance>bott)
              &(Plays[k+1,]$X.Distance<=(topt))){
            throwbin<-j
          }
        }########################
        if (Plays[k+1,]$TO==0){#####PASSES#####
                               Bees[[l]]@ThrCs[catchbin,throwbin]<-Bees[[l]]@ThrCs[catchbin,throwbin]+1
        }
        if (Plays[k+1,]$TO==1){######TOs######
                               Bees[[l]]@ThrOs[catchbin,throwbin]<-Bees[[l]]@ThrOs[catchbin,throwbin]+1      
        }
      }
    }################PICK-UPS################
    if ((Plays[k,]$X.Thrower.id==Bees[[(l)]]@pID)
        &(Plays[k,]$pickup==1)){
      if (Plays[k,]$TO==0){   ##PICKUP PASSES###
        Bees[[l]]@pupCs[1,throwbin]<-Bees[[l]]@pupCs[1,throwbin]+1
      }
      if (Plays[k+1,]$TO==1){ ####PICK-UP TOS###
        Bees[[l]]@pupOs[1,throwbin]<-Bees[[l]]@pupOs[1,throwbin]+1
      }
    }  
  }
  #########################################
  ###GRAPHING
  #########################################
  plotCI(main=namer, 
         xlab="Yards from Endzone", ylab="Yards from midfield",
         xlim=c(1,13.5), ylim=c(1,16.75),
         #cex.main=3, cex.lab=2.5,
         yaxt="n", xaxt="n",
         #########DROPS AND GOALS AT THE TOP#########
         x=((1:13)+0.5), y=rep(15,13), pch=16,
         cex=sqrt(4*(Bees[[l]]@goals)), col="dark green")
png("goals.png", height=1000, width=1000)
hist(Plays[which(Plays$X.Score.==" true"),]$X.X.Position, breaks=1000, xaxt="n")
axis(side=1, at=((1:100)*100), labels=((1:100)*100))
abline(v=7500, col="red")
abline(v=9000, col="red")
abline(v=2500, col="red")
abline(v=1000, col="red")
dev.off()

max()
  
  
  
  
  
  
  namer<-Bees[[l]]@name
  print(namer)
  png(paste(namer, ".png"), width=1000, height=1000)
  par(mar=c(5,7,4,9), pch=16)
  plotCI(main=namer, 
         xlab="Catch Length", ylab="Throw Length    ",
         xlim=c(1,13.5), ylim=c(1,16.75),
         cex.main=3, cex.lab=2.5,
         yaxt="n", xaxt="n",
         #########DROPS AND GOALS AT THE TOP#########
         x=((1:13)+0.5), y=rep(15,13), pch=16,
         cex=sqrt(4*(Bees[[l]]@goals)), col="dark green")
  points(x=((1:13)+0.5), y=rep(16.5,13), pch=16,
         cex=sqrt(4*(Bees[[l]]@drops)), col="dark red")
  text(-1, 15, "GOALS", cex=2, adj=0, xpd=TRUE)
  text(-1, 16.5, "DROPS", cex=2, adj=0, xpd=TRUE)
  abline(h=15.75, col="black")
  abline(h=14.25, col="black")
  
  #############GRAPH FORMATTING###############
  axis(side=1, at=(1:13), labels=bins, cex.axis=2)
  axis(side=2, at=(1:13), labels=bins, cex.axis=2)
  abline(h=4, col="grey")
  abline(v=4, col="grey")
  
  ################PICKUPS#####################
  rect(14, 0.37, 15.4, 14.25, border="black", lwd=1, xpd=TRUE)
  rect(14, 4,    15.4, 4,     border="grey", lwd=1, xpd=TRUE)
  points(x=rep((14.7),13), y=((1:13)+0.5), pch=16,
         cex=sqrt(4*(Bees[[l]]@pupCs)), col="dark blue", xpd=TRUE) 
  points(x=rep((14.7),13), y=((1:13)+0.5), pch=16,
         cex=sqrt(4*(Bees[[l]]@pupOs)), col="red", xpd=TRUE) 
  text(13.8, 0, "PICKUPS", cex=2, adj=0, xpd=TRUE)
  
  ##########MAIN CATCH-THROW PAIRS############
  for (i in 1:13){ 
    points(x=((1:13)+0.5), y=rep((i+0.5),13), pch=16,
           cex=sqrt(4*(Bees[[l]]@ThrCs[,i]+Bees[[l]]@ThrOs[,i])), col="dark blue") 
    points(x=((1:13)+0.5), y=rep((i+0.5),13), pch=16,
           cex=sqrt(4*(Bees[[l]]@ThrOs[,i])), col="red") 
  }
  dev.off()
}