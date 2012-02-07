

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

# Extracting and plotting subgraph for a given player
gP <- generatePlayerSubGraph("Matteo", g)
PlotAssistGraph(gP, Stats)



########################
#### Reading ranking table

Tab <- read.delim("Table.txt")



########################
#### Generating 2D ranking table


Table2D(Tab)

# With alternative table
Table2D(Tab, colBoxTeam="lightblue2", colBoxPoint="cadetblue3", colFontTeam="black", colFontPoints="black")



### 2D ranking tables can also be used to plot scorers/assists stats
Table2D(countGoalsAssists(Stats), pts_col="Goals", team_col="Player", colBoxTeam="goldenrod2", colBoxPoint="black", colFontTeam="black", colFontPoints="white")
Table2D(countGoalsAssists(Stats), pts_col="Assist", team_col="Player", colBoxTeam="lightblue2", colBoxPoint="cadetblue3", colFontTeam="black", colFontPoints="black")

########################
#### Plotting number of points taken

# Default options
pts_taken(Tab)

# With more advanced formatting and alternative color sets
pts_taken(Tab, maxpts=6, colBoxTeam="black", colFontTeam="white", colPointsTaken="goldenrod2", colPointsNotTaken="white")
pts_taken(Tab, maxpts=6, colBoxTeam="darkolivegreen3", colFontTeam="black", colPointsTaken="darkorchid3", colPointsNotTaken="gray90")
pts_taken(Tab, maxpts=6, colBoxTeam="darkolivegreen3", colFontTeam="black", colPointsTaken="darkorange", colPointsNotTaken="gray90")


#########################
#### Alternative barplot to represent number of goals/assists
v <- as.numeric(as.vector(countGoalsAssists(Stats)$Goals))
names(v) <- countGoalsAssists(Stats)$Player
v <- v[v > 0]

barplotS(v)
barplotS(v, fill="goldenrod1", printNb=F)

