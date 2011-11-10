

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

Tab <- read.delim("Table.txt", header=F)


plot(c(0,1), c(0,1),  ann=F, xlim=c(0,10), ylim=c(min(Tab[,2]) - 1, max(Tab[,2]) + 1), bty='n',type='n',xaxt='n',yaxt='n')

for (i in unique(Tab[,2])) {
	
#	pts <- Tab[i,2]
#	team <- Tab[i,1]

	teams <- Tab[which(Tab[,2]==i),1]
	
	sep=""
	teams_text <- ""
	for (te in teams) {
		teams_text <- paste(teams_text, te, sep=sep)
		sep = ", "
	}

	# Drawing rectangles for team and number of points
	rect(xleft=0, ybottom=i, xright=9, ytop=i+1, col="lightcyan1", lty=0)
	rect(xleft=9.1, ybottom=i, xright=10, ytop=i+1, col="cadetblue3", lty=0)
	
	text(x=9.5, y=i+0.5, i, col="black")
	text(x=0, y=i+0.5, teams_text, col="black", pos=4)
	
	}

