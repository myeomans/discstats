library(hexbin)
library(gplots)

bee <- setClass("bee", 
                representation(pID="vector",
                               name="character",
                               Opos="vector",
                               colour="character",
                               goals="vector",
                               drops="vector",
                               ThrCs="array",
                               ThrOs="array",
                               pupCs="vector",
                               pupOs="vector"
                ))

Bees <- list()
for (i in 1:nrow(Player)){
  Bees[[i]] <-bee(pID=Player[i,]$id, 
                  name=as.character(Player[i,]$name),
                  Opos=Player[i,]$Opos,
                  ThrCs=array (),
                  ThrOs=array (),
                  goals=vector(),
                  drops=vector(),
                  pupCs=vector(),
                  pupOs=vector()
  )
}
Bees[[(nrow(Player)+1)]]<-bee(pID=0, 
                              name="EveryBee",
                              Opos=sum(Play$homeOnOffense==1),
                              ThrCs=array (),
                              ThrOs=array (),
                              goals=vector(),
                              drops=vector(),
                              pupCs=vector(),
                              pupOs=vector())

for (l in 1:length(Bees)){
  l<-2
  filterc<-((Play$homeOnOffense==1)&(Play$recieverId>0))
  filtert<-((Play$homeOnOffense==1)&(Play$throwerId>0))
  if (Bees[[(l)]]@pID>0){ ###pID=0 is the whole team
    filterc<-(Play$homeOnOffense==1)&(Play$recieverId==Bees[[(l)]]@pID)
    filtert<-(Play$homeOnOffense==1)&(Play$throwerId ==Bees[[(l)]]@pID)
  } 

  for (k in 1:nrow(Play)){
    if (filterc[k]){ ##IF OUR BEE MADE THIS CATCH###
      ####################################
      if (Play[k,]$score==1){     ####SCORE###
        Bees[[l]]@goals<-c(Bees[[l]]@goals, Play[k,]$rushingDistance)
      }#GOAL
      else if (Play[k,]$drop==1){ ####DROP####
        Bees[[l]]@drops<-c(Bees[[l]]@drops, Play[k,]$rushingDistance)
      }#DROP
      else{                       ####THROW###
        tdist<-Play[k+1,]$rushingDistance
        if (Play[k+1,]$TO==0){#####PASSES#####
                              Bees[[l]]@ThrCs<-cbind(Bees[[l]]@ThrCs, 
                                                     rbind(Play[k,]$rushingDistance, 
                                                     Play[k,]$rushingDistance
        }
        if (Play[k+1,]$TO==1){######TOs######
                              Bees[[l]]@ThrOs[catchbin,throwbin]<-Bees[[l]]@ThrOs[catchbin,throwbin]+1      
        }
      }#THROW
    } #CATCHES
    ################PICK-UPS################
    if (filtert[k]&(Play[k,]$pickup==1)){
      if (Play[k,]$TO==0){   ##PICKUP PASSES###
        Bees[[l]]@pupCs[1,throwbin]<-Bees[[l]]@pupCs[1,throwbin]+1
      }   #PASSES
      if (Play[k+1,]$TO==1){ ####PICK-UP TOS###
        Bees[[l]]@pupOs[1,throwbin]<-Bees[[l]]@pupOs[1,throwbin]+1
      } #TOs
    }
    print(paste(k, " - ", Bees[[l]]@name))}
  #########################################
  ###GRAPHING
  #########################################
  namer<-Bees[[l]]@name
  print(namer)
  png(paste(namer, ".png"), width=1000, height=1000)
  par(mar=c(5,7,4,9), pch=16)
  scaler <- function (x, pos) (70*sqrt(x/pos))
  colfunc <- colorRampPalette(c("firebrick3", "yellow", "blue"))
  smear<-colfunc(50)
  
  plotCI(main=namer, 
         xlab="Catch Length", ylab="Throw Length    ",
         xlim=c(1,13.5), ylim=c(1,16.75),
         cex.main=3, cex.lab=2.5,
         yaxt="n", xaxt="n",
         #########DROPS AND GOALS AT THE TOP#########
         x=((1:13)+0.5), y=rep(15,13), pch=16, xpd=TRUE,
         cex=scaler(Bees[[l]]@goals, Bees[[l]]@Opos), col="dark green")
  points(x=((1:13)+0.5), y=rep(16.5,13), pch=16,
         cex=scaler(Bees[[l]]@drops, Bees[[l]]@Opos), col="firebrick1")
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
  points(x=rep((14.7),13), y=((1:13)+0.5), pch=16, xpd=TRUE,
         cex=scaler(Bees[[l]]@pupCs+Bees[[l]]@pupOs, Bees[[l]]@Opos), 
         col=smear[round(50*Bees[[l]]@pupCs/(Bees[[l]]@pupCs+Bees[[l]]@pupOs), 0)])
  text(13.8, 0, "PICKUPS", cex=2, adj=0, xpd=TRUE)
  ##########MAIN CATCH-THROW PAIRS############
  for (i in 1:13){     
    points(x=((1:13)+0.5), y=rep((i+0.5),13), pch=20,
           cex=scaler((Bees[[l]]@ThrCs[,i]+Bees[[l]]@ThrOs[,i]), Bees[[l]]@Opos), 
           col=smear[round(50*Bees[[l]]@ThrCs[,i]/(Bees[[l]]@ThrCs[,i]+Bees[[l]]@ThrOs[,i]), 0)]) 
  }
  ##########LEGEND AND POSS COUNT#############
  points(x=11, y=11, pch=16,
         cex=scaler(1, 100), col="black")
  text(12.5, 11, "= 1/100 poss", cex=2, adj=0.5, xpd=TRUE)
  rect(11.5, 11.75, 13.75, 14, border="black", col="white", lwd=1, xpd=TRUE)
  text(12.625, 13.25, Bees[[l]]@Opos, cex=3, adj=0.5, xpd=TRUE)
  text(12.625, 12.25, "O POSS", cex=2.2, adj=0.5, xpd=TRUE)
  points(x=(11+(0:5)*0.2), y=rep(10, 6), pch=16,
         cex=scaler(1, 200), col=smear[c(1, 10, 20, 30, 40, 50)])
  dev.off()
}