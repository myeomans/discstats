playerstats<- function(teamdata){
  Player<-teamdata$Player

  # MUST FIX

  #Player$Dees       <-round(Player$      /Player$Touches, 2)

  Player$YardsThrown<-unlist(lapply(Player$id, function(x) round(sum(Play[which(Play$throwerId==x),]$rushingDistance), 2)))
  Player$YardsCaught<-unlist(lapply(Player$id, function(x) round(sum(Play[which(Play$recieverId==x),]$rushingDistance), 2)))
  Player$Throws     <-unlist(lapply(Player$id, function(x) sum(Play$throwerId==x)))
  Player$pickups    <-unlist(lapply(Player$id, function(x) sum((Play$throwerId==x)&(Play$pickup==1))))
  Player$TO         <-unlist(lapply(Player$id, function(x) sum((Play$throwerId==x)&(Play$TO==1))))
  Player$Assists    <-unlist(lapply(Player$id, function(x) sum((Play$throwerId==x)&(Play$score==1))))
  Player$Drops      <-unlist(lapply(Player$id, function(x) sum((Play$recieverId==x)&(Play$drop==1))))
  Player$Goals      <-unlist(lapply(Player$id, function(x) sum((Play$recieverId==x)&(Play$score==1))))
  Player$YardsAll   <-round(Player$YardsCaught+Player$YardsThrown, 2)
  Player$Touches    <-round(Player$Drops+Player$Goals+Player$Throws, 2)
  Player$Turns      <-round(Player$Drops+Player$TO, 2)
  Player$YdsThrPP   <-round(Player$YardsThrown/Player$Opos, 2)
  Player$YdsRecPP   <-round(Player$YardsCaught/Player$Opos, 2)
  Player$YdsAllPP   <-round(Player$YardsAll   /Player$Opos, 2)
  Player$TurnsPP    <-round(Player$Turns      /Player$Opos, 2)
  Player$AssistsPP  <-round(Player$Assists    /Player$Opos, 2)
  Player$GoalsPP    <-round(Player$Goals      /Player$Opos, 2)
  Player$AssGoalPP  <-round(Player$AssistsPP+Player$GoalsPP, 2)
  Player$YdsThrPT   <-round(Player$YardsThrown/Player$Touches, 2)
  Player$YdsRecPT   <-round(Player$YardsCaught/Player$Touches, 2)
  Player$YdsAllPT   <-round(Player$YardsAll   /Player$Touches, 2)
  Player$TurnsPT    <-round(Player$Turns      /Player$Touches, 2)
  Player$TurnsPT    <-round(Player$Turns      /Player$Touches, 2)
  Player$DsPP       <-round(Player$Dees       /Player$Touches, 2)
  teamdata$Player<-Player
  return(teamdata)
}
