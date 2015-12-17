mean(Play[which((Play$SL==0)),]$TO)
mean(Play[which((Play$SL==1)),]$TO)
mean(Play[which((Play$SLR==1)),]$TO)
mean(Play[which((Play$SLL==1)),]$TO)

Play$unDTO<-resid(glm(TO~totalDistance, data=Play, family=binomial))
mean(Play[which((Play$SL==0)),]$unDTO)
mean(Play[which((Play$SL==1)),]$unDTO)
mean(Play[which((Play$SLR==1)),]$unDTO)
mean(Play[which((Play$SLL==1)),]$unDTO)


summary(lm(unDSL~SL, data=Play))


names<-list()
names[[1]]<- "straight 0-15 yards"
names[[2]]<- "straight 15-30 yards"
names[[3]]<- "straight 30+ yards"
names[[4]]<- "swings 15+ yards"

CUT<-list()
CUT[[1]]<-((abs(Play$LRspan)<10)&(Play$FWspan<15)) #SHORT
CUT[[2]]<-((abs(Play$LRspan)<10)&(Play$FWspan>15)
                                &(Play$FWspan<30)) #MED
CUT[[3]]<-((abs(Play$LRspan)<10)&(Play$FWspan>30)) #LONG
CUT[[4]]<-((abs(Play$LRspan)>15)&(Play$FWspan<15)) #SWINGS

for (i in 1:4){
count<-array(NA, c(2,3))
count[1,1]<-nrow(Play[which(((Play$TO==0)&(Play$plane==0)&CUT[[i]])),])
count[1,2]<-nrow(Play[which(((Play$TO==1)&(Play$plane==0)&CUT[[i]])),])
count[2,1]<-nrow(Play[which(((Play$TO==0)&(Play$plane==1)&CUT[[i]])),])
count[2,2]<-nrow(Play[which(((Play$TO==1)&(Play$plane==1)&CUT[[i]])),])
count[1,3]<-count[1,2]/(count[1,1]+count[1,2])
count[2,3]<-count[2,2]/(count[2,1]+count[2,2])
rownames(count)<-c("control", "goal line")
colnames(count)<-c("compl", "TO", "PCT")
print(names[[i]])
print(count)
}

summary(glm(TO~plane*totalDistance, data=Play[which(CUT),], family=binomial))