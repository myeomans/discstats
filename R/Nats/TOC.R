install.packages("shape")
library(shape)

#THIS IS THE FOLDER NAME - YOU SHOULD CHANGE IT
setwd("C:/Users/Mike/Google Drive/Frisbee/Nats/")

#READS IN DATA
Play<-    read.csv("ALLDATA.csv")

source("formatter.R")
#source("possessions.R") TAKES A LONG TIME!!
source("endzones.R")


source("drawatlas.R")
source("drawfield.R")
source("plotprep.R")
source("PLAYERS.R")

#THREE AT A TIME GRAPHS
source("catchfieldpanels.R")
source("throwfieldpanels.R")
source("passatlaspanels.R")

#ONE AT A TIME GRAPHS
source("passatlas.R")
#source("heatatlas.R")
source("throwfield.R")
source("catchfield.R")

#source("SLregressions.R",         sep=""))

#MUST FIX Line AND Player FILES
#source( "filestitch.R", ) #BROKEN SYSTEM
#Play<-    read.csv( "eliteMens2013newPlays.csv", sep=""))
#Players<- read.csv( "eliteMens2013newPlayers.csv", sep=""))
#source("PossessionCounts.R",      sep=""))
