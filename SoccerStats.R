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




##########  Generate a subgraph for a given player (need to first generate the global graph)

generatePlayerSubGraph <- function(player, g) {

  if (!(player %in% nodes(g))) {
      cat("Error:", player ,"not in specified graph"); return(FALSE);
  }
  
  
  snodes <- c(player, edges(g)[[player]])
  
  for (e in names(edges(g))) {
    if (player %in% edges(g)[[e]]) {
      snodes <- c(snodes, e)
    }
  }

  return(subGraph(unique(snodes), g))

}




### Should add a legend
PlotAssistGraph <- function(g, Stats, emphasize="both", lowColNodes="white", highColNodes="steelblue3", NodesFontColor="black", NodeFontSize=22, EdgeFontSize=22, NodeFixedSize=FALSE) {

	if (!(emphasize %in% c("both", "goals", "assists"))) { 
		cat("Error - unknown value for emphasize parameter")
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
	colscale_edges <- colorpanel(n=max(edW), low="gray30", high="gray0")
	names(colscale_edges) <- 1:max(edW)
	
	edgesColor <- colscale_edges[edW]
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

countGoalsAssists <- function(Stats) {
  
  scorers <- Stats$Goal	
  stats_goals <- sort(summary(scorers), decreasing=T)

  assists <- Stats$Assist[Stats$Assist!="NULL"]
  assists <- as.factor(as.vector(assists))
  stats_pass <- sort(summary(assists), decreasing=T)
  
  players <- unique(c(names(stats_goals), names(stats_pass)))
  res <- matrix(0, ncol=2, nrow=length(players), dimnames=list(players, c("Goals","Assist")))
  
  res[names(stats_goals),"Goals"] <- stats_goals
  res[names(stats_pass),"Assist"] <- stats_pass
  res <- cbind(rownames(res), res)
  res <- as.data.frame(res)
  colnames(res)[1] <- "Player"
  return(res)
  }





Table2D <- function(Tab, colBoxTeam="grey90", colBoxPoint="black", colFontTeam="black", colFontPoints="white", pts_col="pts", team_col="Team") {
	
	if (!(pts_col %in% colnames(Tab)) | !(team_col %in% colnames(Tab))) { 
		print("Error. Input object should have 'Team' and 'pts' columns")
		return(FALSE)
	}
	
  Tab <- Tab[order(as.numeric(as.vector(Tab[,pts_col])), decreasing=T), ]
	plot(c(0,1), c(0,1),  ann=F, xlim=c(0,10), ylim=c(min(as.numeric(as.vector(Tab[,pts_col]))) - 1, max(as.numeric(as.vector(Tab[,pts_col]))) + 1.5), bty='n',type='n',xaxt='n',yaxt='n')
	
	# First, plotting headers
	k <- max(as.numeric(as.vector(Tab[,pts_col]))) + 1.2
	rect(xleft=9.1, ybottom=k, xright=10, ytop=k+1, col=colBoxPoint, lty=0)
	rect(xleft=1.2, ybottom=k, xright=9, ytop=k+1, col=colBoxTeam, lty=0)
	rect(xleft=0, ybottom=k, xright=1, ytop=k+1, col=colBoxPoint, lty=0)	

	text(x=9.5, y=k+0.45, pts_col, col=colFontPoints)
	text(x=1.2, y=k+0.45, team_col, col=colFontTeam, pos=4)
	text(x=0.4, y=k+0.45, "#", col=colFontPoints)
	
	# Then, plotting boxes with points
	for (k in min(as.numeric(as.vector(Tab[,pts_col]))):max(as.numeric(as.vector(Tab[,pts_col])))) {
		
		rect(xleft=9.1, ybottom=k, xright=10, ytop=k+1, col=colBoxPoint, lty=0)
		text(x=9.5, y=k+0.45, k, col=colFontPoints)
	}
	
	# Now plotting teams and ranks
	ranks <- rank(-as.numeric(as.vector(Tab[,pts_col])), ties.method="min")
	
	for (i in unique(as.numeric(as.vector(Tab[,pts_col])))) {
		

		
		teams <- as.vector(Tab[which(as.numeric(as.vector(Tab[,pts_col]))==i),team_col])
		team_index <- which(teams[1] == as.vector(Tab[,team_col]))
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
		text(x=1.2, y=i+0.4, teams_text, col=colFontTeam, pos=4)
		text(x=0.4, y=i+0.45, thisRank, col=colFontPoints)
		
		}

	
}





pts_taken <- function(Tab, maxpts=6, colBoxTeam="gray85", colFontTeam="black", colPointsTaken="cadetblue3", colPointsNotTaken="gray95") {

  if (!("pts_taken" %in% colnames(Tab)) | !("Team" %in% colnames(Tab))) { 
	  print("Error. Input object should have 'Team' and 'pts_taken' columns")
	  return(FALSE)
  }
  
  
  nb_teams <- dim(Tab)[1]
  plot(c(0,1), c(0,1),  ann=F, xlim=c(0,max(Tab[,"pts_taken"] + 9)), ylim=c(0,nb_teams + 2), bty='n',type='n',xaxt='n',yaxt='n')
  
  team_ys <- nb_teams:1
  for (i in 1:dim(Tab)[1]) {
    
    # Plotting rank
    rect(xleft=0, ybottom=team_ys[i]+0.1, xright=0.9, ytop=team_ys[i]+0.9, col=colBoxTeam, lty=0)
    text(x=0, y=team_ys[i]+0.4, i, col=colFontTeam, pos=4) 
    
    # Plotting team name
    rect(xleft=1, ybottom=team_ys[i]+0.1, xright=7, ytop=team_ys[i]+0.9, col=colBoxTeam, lty=0)
    text(x=1.1, y=team_ys[i]+0.4, Tab[i,"Team"], col=colFontTeam, pos=4)
    
    # Defining points rectangles coordinates
    rects_xleft <- 6 + 1:maxpts  
    rects_xright <- rects_xleft + 0.9
    rects_xleft <- rects_xleft + 0.1
    
    # Defining colors of rectangles
    rect_cols <- 1:maxpts
    rect_points <- rect_cols
    rect_cols[rect_points <= Tab[i,"pts_taken"]] <- colPointsTaken
    rect_cols[rect_points > Tab[i,"pts_taken"]] <- colPointsNotTaken
    
    # Plotting
    rect(xleft=rects_xleft, ybottom=team_ys[i] + 0.1, xright=rects_xright, ytop=team_ys[i]+0.9, col=rect_cols, lty=0)

    
  }

  
}





##### Enhanched barplots

barplotS <- function(v, fill="cadetblue3", labels=names(v), sizeNames=5, printNb=T) {
  
  v <- sort(v, decreasing=F)
  plot(0, col="white", ann=F, bty='n',type='n',xaxt='n',yaxt='n', xlim=c(0,max(v) + sizeNames) , ylim=c(0,length(v)))
  row = 0
  for (i in 1:length(v)) {
    
    ybot <- rep(row + 0.1, length(v))
    ytop <- rep(row + 0.9, length(v)) 
    
    xleft <- seq(0,v[i] - 1) + 0.1 + sizeNames
    xright <- seq(1,v[i]) - 0.1 + sizeNames
    rect(xleft, ybot, xright, ytop, col=fill[1], border=fill[1])
      
    row <- row + 1
    }

  ### Adding names
  if (printNb) {
    labels <- paste(labels, " (", v, ")", sep="")
    } 
  text(rep(0.5, length(labels)), seq(1,length(labels)) - 0.5, labels=labels, pos=4)
  

}
