library(graph)
library(Rgraphviz)
library(gplots)




GenerateAssistGraph <- function(Stats) {
	
	nodes <- unique(as.vector(unlist(Stats[,1:2])))
	nodes <- nodes[nodes!="NULL"]
	edges <-list()
	
	
	g <- new("graphNEL", nodes, edges, "directed")
	edgeDataDefaults(g, "weight") <- 1
	
	for (i in 1:dim(Stats)[1]) {
		goal <- as.vector(Stats[i,1])
		pass <- as.vector(Stats[i,2])
		if (pass != "NULL") {
			# If the edge does not exist we create it
			# Else, we just update the edge weight
			
			if(goal %in% edges(g)[[pass]]) {
				edgeData(g, from=pass, to=goal, attr="weight") <- edgeData(g, from=pass, to=goal, attr="weight")[[1]] + 1
			} else {
				g <- addEdge(as.vector(Stats[i,2]), as.vector(Stats[i,1]), g)
			}
		}
		
	}
	

	return(g)

}



PlotAssistGraph <- function(g, Stats, emphasize="both", lowColNodes="white", highColNodes="steelblue3", NodesFontColor="black", NodeFontSize=22, EdgeFontSize=22, NodeFixedSize=FALSE) {

	if (!(emphasize %in% c("both", "goals", "assists"))) { 
		print("Error - unknown value for emphasize parameter")
		return(FALSE)
	}

	### Extracting useful stats from Stats object
	scorers <- Stats$Goal
	assists <- Stats$Assist[Stats$Assist!="NULL"]
	assists <- as.factor(as.vector(assists))
	
	stats_goals <- sort(summary(scorers), decreasing=T)
	stats_pass <- sort(summary(assists), decreasing=T)




	######## Storing in a vector the nb of goals
	nb_goals <- stats_goals[nodes(g)]
	names(nb_goals)[which(is.na(names(nb_goals)))] <- setdiff(nodes(g), scorers)
	nb_goals[is.na(nb_goals)] <- 0

	######## Storing in a vector the nb of passes
	nb_pass <- stats_pass[nodes(g)]
	nb_pass[is.na(nb_pass)] <- 0
	names(nb_pass) <- nodes(g)
	
	######## Computing number of contributions (goals + assists)
	nb_contrib <- nb_goals + nb_pass
	
	### Generating the labels of the nodes for the final plot
	labels <- paste(names(nb_goals), " (", nb_goals, ") ", "(", nb_pass, ")", sep="")


	######## Generating a color scale for nodes

	
	if (emphasize=="goals") {object <- nb_goals}
	if (emphasize=="assists") {object <- nb_pass}
	if (emphasize=="both") {object <- nb_contrib}
	
	colscale_nodes <- colorpanel(n=max(object) + 1, low=lowColNodes, high=highColNodes)
	names(colscale_nodes) <- 0:max(object)
	
	
	fills <- colscale_nodes[as.character(object)]
	names(fills) <- nodes(g)
	names(labels) <- nodes(g)


	
	######## Retrieving the weight of edges
	weightAttrAllEdges <- edgeData(g, attr="weight")
	edW <- as.vector(unlist(weightAttrAllEdges))
	names(edW) <- sub("|","~",names(weightAttrAllEdges), fixed=T)
	
	
	######## Defining the color of the edges according their weight
	colscale_edges <- colorpanel(n=max(edW), low="gray70", high="gray0")
	names(colscale_edges) <- 1:max(edW)
	
	edgesColor <- colscale2[edW]
	names(edgesColor) <- sub("|","~",names(weightAttrAllEdges), fixed=T)
	
	
	######## Assigning number of passes as labels for the edges
	
	edgesLabel <- edW
	edgesLabel[edgesLabel == 1] <- ""
	names(edgesLabel) <- names(edW)
	
	
	######## Creating a vector containing the font size of nodes
	fsize <- rep(NodeFontSize, length(nodes(g)))
	names(fsize) <- nodes(g)

	######## Creating a vector containing the font color of nodes
	fCols <- rep(NodesFontColor, length(nodes(g)))
	names(fCols) <- nodes(g)
		
	######## Creating a vector indicating whether nodes should be scaled in function of the node label
	fixedsize <- rep(NodeFixedSize, length(nodes(g)))
	names(fixedsize) <- nodes(g)
	
	######## Creating a vector to set up the font size of the edges labels
	edgesFontSize <- rep(EdgeFontSize, length(edgesLabel))
	names(edgesFontSize) <- names(edgesLabel)
	
	####### Plotting
	plot(g, recipEdges = "distinct",  nodeAttrs=list(fillcolor=fills, label=labels, fontsize=fsize, fixedsize=fixedsize, fontcolor=fCols), edgeAttrs=list(label=edgesLabel, labelfontsize=edgesFontSize, color=edgesColor))
	

}













plotGoals <- function(Stats, col="steelblue3", horiz=T, las=1,...) {
	
	scorers <- Stats$Goal	
	stats_goals <- sort(summary(scorers), decreasing=T)
	
	barplot(rev(stats_goals), col=col, horiz=horiz, las=las, ...)
	
	}



plotAssists <- function(Stats, col="steelblue3", horiz=T, las=1, ...) {
	
	assists <- Stats$Assist[Stats$Assist!="NULL"]
	assists <- as.factor(as.vector(assists))
	stats_pass <- sort(summary(assists), decreasing=T)
	
	barplot(rev(stats_pass), col=col, horiz=horiz, las=las, ...)
	
	}



Table2D <- function(Tab, colBoxTeam="grey90", colBoxPoint="black", colFontTeam="black", colFontPoints="white") {
	
	if (!("pts" %in% colnames(Tab)) | !("Team" %in% colnames(Tab))) { 
		print("Error. Input object should have 'Team' and 'pts' columns")
		return(FALSE)
	}
	
	plot(c(0,1), c(0,1),  ann=F, xlim=c(0,10), ylim=c(min(Tab[,"pts"]) - 1, max(Tab[,"pts"]) + 1.5), bty='n',type='n',xaxt='n',yaxt='n')
	
	# First, plotting headers
	k <- max(Tab[,"pts"]) + 1.2
	rect(xleft=9.1, ybottom=k, xright=10, ytop=k+1, col=colBoxPoint, lty=0)
	rect(xleft=1.2, ybottom=k, xright=9, ytop=k+1, col=colBoxTeam, lty=0)
	rect(xleft=0, ybottom=k, xright=1, ytop=k+1, col=colBoxPoint, lty=0)	

	text(x=9.5, y=k+0.35, "Pts", col=colFontPoints, vfont=c("sans serif", "bold"))
	text(x=1.2, y=k+0.35, "Teams", col=colFontTeam, pos=4, vfont=c("sans serif", "bold"))
	text(x=0.4, y=k+0.35, "#", col=colFontPoints, vfont=c("sans serif", "bold"))
	
	# Then, plotting boxes with points
	for (k in min(Tab[,"pts"]):max(Tab[,"pts"])) {
		
		rect(xleft=9.1, ybottom=k, xright=10, ytop=k+1, col=colBoxPoint, lty=0)
		text(x=9.5, y=k+0.45, k, col=colFontPoints, vfont=c("sans serif", "bold"))
	}
	
	# Now plotting teams and ranks
	ranks <- rank(-Tab[,"pts"], ties.method="min")
	
	for (i in unique(Tab[,"pts"])) {
		

		
		teams <- Tab[which(Tab[,"pts"]==i),1]
		team_index <- which(teams == Tab[,"Team"])
		thisRank <- ranks[team_index][1]
		
		sep=""
		teams_text <- ""
		for (te in teams) {
			teams_text <- paste(teams_text, te, sep=sep)
			sep = ", "
		}
	
		# Drawing rectangles for team and position
		rect(xleft=1.2, ybottom=i, xright=9, ytop=i+1, col=colBoxTeam, lty=0)
		rect(xleft=0, ybottom=i, xright=1, ytop=i+1, col=colBoxPoint, lty=0)
		
		# Writing the number of points and the teams' names
		text(x=1.2, y=i+0.4, teams_text, col=colFontTeam, pos=4, vfont=c("sans serif", "bold"))
		text(x=0.4, y=i+0.45, thisRank, col=colFontPoints, vfont=c("sans serif", "bold"))
		
		}

	
}








