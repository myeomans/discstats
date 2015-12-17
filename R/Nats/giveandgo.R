require(compiler)
enableJIT(3)

Play$throwrec<-NA
Play$GG<-0
Play$GGchain<-0
for (p in 2:nrow(Play)){
  Play[p,]$throwrec<-paste(sort(c(Play[p,]$throwerId, Play[p,]$recieverId)), collapse=" ")
  Play[p,]$GG<-1*((Play[p,]$recieverId==Play[p-1,]$throwerId)
                 &(Play[p,]$pointId   ==Play[p-1,]$pointId)
                 &!(Play[p,]$recieverId==0)
                 &!(Play[p,]$throwerId==0))
  Play[p,]$GGchain<-Play[p,]$GG*(Play[p-1,]$GGchain+1)
print(p)}

BW<- paste(1378345407, 1378346043)
which((Play$throwrec==BW)&(Play$GG==1))