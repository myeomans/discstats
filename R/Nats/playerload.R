######################################
#FIXING DUPLICATE PLAYERS IN PLAYS
######################################
dupes<-array(c(1376145998, 1372267864, #case  ->Casey
               1377348372, 1377349639, #adam  ->Adam
               1377348273, 1377349561, #danny ->Danny
               1377348343, 1377349594, #yiding->Yiding
               1377348372, 1377349639, #adam  ->Adam
               1371016296, 0,          #DEAN OUT
               1372267817, 0),         #DOUG OUT
             c(2,7))

for (i in 1:7){
  old<-dupes[1,i]
  new<-dupes[2,i]
  if(      sum(Play$throwerId   ==old)){
    Play[which(Play$throwerId   ==old),]$throwerId <-new
  }
  if(      sum(Play$recieverId  ==old)){
    Play[which(Play$recieverId  ==old),]$recieverId<-new
  }
  if(      sum(Line$playerId    ==old)){
    Line[which(Line$playerId    ==old),]$playerId  <-new  
  }
}
######################################
#Turns Luke into Yeomans at CHC
old<-1372267668
new<-1371016154
CHCP<-(as.numeric(Play$gameId)>=(1377349714))
CHCL<-(as.numeric(Line$gameId)>=(1377349714))
Play[which(CHCP&Play$throwerId ==old),]$throwerId <-new
Play[which(CHCP&Play$recieverId==old),]$recieverId<-new
Line[which(CHCL&Line$playerId  ==old),]$playerId  <-new
Player$name<-as.character(Player$name)
Player[which(Player$id==1372267668),]$name<-"Luke"
######################################
#RE-EDIT PLAYER LIST
######################################
ghosts<- c(1372267817, #DEAN 
           1371016296, #DOUG
           1377348372, #ADAM
           1376145998, #CASE
           1377348273, #DANNY
           1377348343) #YIDING

Player<-Player[-which(Player$id %in% ghosts),]
######################################
#CORRECT SPECIFIC PLAY ERRORS
######################################
Pfinder <- function (x, y) which((Play$gameId==x)&(Play$playId==y))
#RAGNAROK GHOST TO
Play[Pfinder(1373048112,33),]$recieverId<-0
Play[Pfinder(1373048112,33),]$throwerId<-0
#SAM GO TO
Play[Pfinder(1377349714, 116),]$TO<-0
Play[Pfinder(1377349714, 116),]$drop<-1
Play[Pfinder(1377349714, 116),]$recieverId<-1372268222
#JOE'S BOMB IN OAKS
Play[Pfinder(1377454468, 231),]$recieverId<-1371016236

#GOALS NOT IN THE ENDZONE
#which(Play$score==1&Play$homeOnOffense==1&Play$EZdist<0) #FOUR OUTLIERS
#1376145881 1376145881 1377371921 1378566081
#5861 6270 2582 6928

#CASTLE 6-4 OAKS INJURY SUBS
#FERGUS TO KURT HALFWAY
#BK FOR JOE LAST POSSESSION
#Play[Pfinder(1377371921, ),]

#Doubewide
#Point #3 - matt west had a D, sam had a throwaway. 
#Point #15 - granite had the unassigned TO
#Yeomans had only one touch, no turnovers

#Euforia
#one unassigned catch and throwaway to Yeomans
#11-8 Matt West to Zubair injury sub

#Ring of Fire
#Unassigned throws go to Yeomans
#7-4  Reilly's stats go to Kurt
#7-4  Uber replaced Yngve after a few turnovers
#10-5 Bruns' stats go to Kurt. 

#Mephisto
#13-11 Joe dropped, not Kurt

#Ragnarok
#7-4 Fergus, not Marty (3 touches, +5 -5 yc, +10 -5 yt)

#Revolver
#??0-0 2 passes, then a huck
#3-1 Matt West, not Granite (+5 yc +1G)

#Ironside
#10-5 Grant, not Jimmy W (+5 YC, +1G)

#JURRASIC SHARK
#Second Point was not scored by them!!! 
#JS throwaway before midfield
#Roush picks it up, throws a 5 yard swing right to Adam
#Adam hits roush in the front right corner