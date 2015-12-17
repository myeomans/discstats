
PID<-array(NA, c(length(unique(Play$throwerId)), 4))
PID[,1]<-unique(Play$throwerId)
for (ii in 1:nrow(PID)){
  PID[ii,2]<-mean(Play[which((Play$throwerId==PID[ii,1])
                            &(Play$plane==0)
                            &(Play$FWspan<20)
                            &(Play$FWspan>0)),]$TO, na.rm=T)*100
  PID[ii,3]<-mean(Play[which((Play$throwerId==PID[ii,1])
                            &(Play$plane==1)
                            &(Play$FWspan<20)
                            &(Play$FWspan>0)),]$TO, na.rm=T)*100
  PID[ii,3]<-SEM  (Play[which((Play$throwerId==PID[ii,1])
                             &(Play$plane==1)
                             &(Play$FWspan<20)
                             &(Play$FWspan>0)),]$TO, na.rm=T)*100
  PID[ii,4]<-length(   which((Play$throwerId==PID[ii,1])
                             &(Play$plane==1)))
print(ii)}
PID<-PID[which(PID[,4]>15),]
PID<-PID[which(PID[,4]<1000),]
nrow(PID)



par(mar=c(4,4,2,2))
plot(x=PID[,2], y=PID[,3], cex=sqrt(PID[,4]/3),
     xlab="All Turnover %", ylab="Assist Attempt TO %",
     xlim=c(0,30), ylim=c(0,30))
abline(a=0, b=1, col="grey")


points(x=mean(Play[which((Play$plane==0)
                        &(Play$FWspan<20)
                        &(Play$FWspan>0)),]$TO, na.rm=T)*100,
       y=mean(Play[which((Play$plane==1)
                        &(Play$FWspan<20)
                        &(Play$FWspan>0)),]$TO, na.rm=T)*100,
       col="red")

length((Play[which((Play$plane==1)
                  &(Play$homeOnOffense==1)
                  &(Play$FWspan<20)
                  &(Play$FWspan>0)),]$TO))