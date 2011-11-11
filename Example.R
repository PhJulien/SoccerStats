

########################
#### Loading the SoccerStats module which contain all the functions
#### Be careful to specifiy the correct (relative of absolute) file path

source("SoccerStats.R")

########################
#### Reading input file
#### Be careful to specifiy the correct (relative of absolute) file path

Stats <- read.delim("GoalsAssists.txt")


########################
#### Plotting number of goals and assists per player

plotGoals(Stats)

plotAssists(Stats)

# On a same device
par(mfrow=c(2,1))
plotGoals(Stats)
plotAssists(Stats)


########################
#### Generating a graph of goals and assists

g <- GenerateAssistGraph(Stats)

########################
#### Using this object to plot the goals and assists stats of them team

# Using defaut parameters
PlotAssistGraph(g, Stats)

# And custom ones
PlotAssistGraph(g, Stats, emphasize="goals", lowColNodes="white", highColNodes="goldenrod2", NodesFontColor="black", NodeFontSize=22, EdgeFontSize=25, NodeFixedSize=FALSE)






########################
#### Reading ranking table

Tab <- read.delim("Table.txt")



########################
#### Generating 2D ranking table

Table2D(Tab)




