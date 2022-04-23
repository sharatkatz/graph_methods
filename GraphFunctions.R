##################################################################################################
# Graph functions
##################################################################################################
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}

# create nodes
GraphNodeListCreate <- function(ingraph, mode='all') {
    nodes_ <- melt(ingraph, id.vars=c())
    nodes_ <- data.frame(value = nodes_[!((nodes_$value=="") | is.na(nodes_$value)) , ]$value)
    nodes_ <- unique(nodes_)
        return (nodes_)
}

# create edges
GraphEdgeListCreate <- function(ingraph2, id.vars) {
    edata <- melt (ingraph2, id.vars=id.vars)
    clean_ <- unique(na.omit(blankToNA(edata)))
    clean_ <- as.data.frame(sapply(clean_, trim))
    colnames(clean_) <- c("from", "to")
     rm (edata); gc()
     return (clean_)
}

getGraphObject <- function ( ind, from_, to_ ) {
  id_vars_ <- c(from_, to_)
  edge_ <- GraphEdgeListCreate ( ind[, id_vars_], id.vars=id_vars_ )
  node_ <- GraphNodeListCreate ( ind[, id_vars_] )
  node_ <- data.frame (lapply(node_, as.character), stringsAsFactors=FALSE)
  gObject <- graph_from_data_frame(d=edge_, vertices=node_, directed=F)
   return (gObject)
}


# Check for outliers:
graphRemoveOutliers <- function (ing, visual="N", remove="N") {
  out_ <- outliers::outlier(clusters(ing)$csize, opposite = FALSE, logical = FALSE)
  # visualize
  if (toupper(visual)=="Y") {
    c_size <- clusters(ing)$csize
    plot(c_size)
    qplot(seq_along(c_size), c_size)
  }

  ifrm(c_size); gc()
  cat ("|--- NOTE: Outlier found in this graph object has vertices -- ", out_, ")", "=", floor(out_), "----|", "\n")
  cl <- clusters(ing)

  # delete common vertices of outlier cluster size
  if (toupper(remove)=="Y") {
      # loop through to extract common vertices
      common_vert <- lapply(seq_along(cl$csize)[cl$csize == out_],
                        function(x) V(ing)$name[cl$membership %in% x])
      ifrm (out_)
      outg <- delete.vertices (ing, unlist(common_vert))
      return (outg)
    }
   else { return (ing) }

}

# Check for outliers:
graphRemoveOutliers.2 <- function (ing, visual="N", remove, out_) {
  out_ <- out_
  # visualize
  if (toupper(visual)=="Y") {
    c_size <- clusters(ing)$csize
    plot(c_size)
    qplot(seq_along(c_size), c_size)
  }

  ifrm(c_size); gc()
  cat (" |--- NOTE: Outlier found in this graph object has vertices >= ", "floor(", out_, ")", "=", floor(out_), "----|", "\n")
  cl <- clusters(ing)

  # delete common vertices of outlier cluster size
  if (toupper(remove)=="Y") {
      # loop through to extract common vertices
      common_vert <- lapply(seq_along(cl$csize)[cl$csize >= out_],
                        function(x) V(ing)$name[cl$membership %in% x])
      ifrm (out_)
      outg <- delete.vertices (ing, unlist(common_vert))
      return (outg)
    }
   else { return (ing) }

}


# Reference - igraph wikidot
# 1. Clique percolation
# Clique percolation is a community detection method.
# This algorithm is not implemented in igraph, but here is a quick (and rather inefficient) version to do it:
clique.community <- function(graph, k) {
   clq <- cliques(graph, min=k, max=k)
   edges <- c()
   for (i in seq_along(clq)) {
     for (j in seq_along(clq)) {
       if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
         edges <- c(edges, c(i,j)-1)
       }
     }
   }
   clq.graph <- simplify(graph(edges))
   V(clq.graph)$name <- seq_len(vcount(clq.graph))
   comps <- decompose.graph(clq.graph)

   lapply(comps, function(x) {
     unique(unlist(clq[ V(x)$name ]))
   })
}


# 2. Label propagation algorithm by Raghavan et al.
# Usha Nandini Raghavan, Réka Albert and Soundar Kumara. 2007. Near linear time algorithm to detect
# community structures in large-scale networks, Phys. Rev. E 76, 036106 Arxiv
largeScaleCommunity <- function(g,mode="all"){
  V(g)$group <- as.character(V(g))
  thisOrder <- sample(vcount(g),vcount(g))-1
  t <- 0
  done <- FALSE
  while(!done){
    t <- t+1
    cat("\rtick:",t)
    done <- TRUE ## change to FALSE whenever a node changes groups
    for(i in thisOrder){
      ## get the neighbor group frequencies:
      groupFreq <- table(V(g)[neighbors(g,i,mode=mode)]$group)
      ## pick one of the most frequent:
      newGroup <- sample(names(groupFreq) [groupFreq==max(groupFreq)],1)
      if(done){done <- newGroup==V(g)[i]$group}
      V(g)[i]$group <- newGroup
    }
  }
  ## now fix any distinct groups with same labels:
  for(i in unique(V(g)$group)){
    ## only bother for connected groups
    if(!is.connected(subgraph(g,V(g)[group==i]))){
      theseNodes <- V(g)[group==i]
      theseClusters <- clusters(subgraph(g,theseNodes))
      ## iterate through the clusters and append their names
      for(j in unique(theseClusters$membership)){
        V(g)[theseNodes[theseClusters$membership==j]]$group <- paste(i,j,sep=".")
      }
    }
  }
  return(g)
}


# Testing the significance of a community
# The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Call the edges within a community "internal"
# and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external"
# edges incident to a vertex of the community. More internal than external edges show that the community
# is significant; less internal than external edges show that the community is in fact an "anti-community".
# The p-value of the test performed by this function will be close to zero in both cases; the value of the
# test statistic tells us whether we have a community or an anti-community.
community.significance.test <- function(graph, vs, ...) {
    if (is.directed(graph)) stop("This method requires an undirected graph")
    subgraph <- induced.subgraph(graph, vs)
    in.degrees <- degree(subgraph)
    out.degrees <- degree(graph, vs) - in.degrees
    wilcox.test(in.degrees, out.degrees, ...)
}

# USE CASE
#' # 1) generate an vector consisting of random values from beta distribution
#' x <- rbeta(1000, shape1=0.5, shape2=1)
#'
#' # 2) fit a p-value distribution under beta-uniform mixture model
#' fit <- dBUMfit(x, ntry=1, hist.bum=FALSE, contour.bum=FALSE)
#'
#' # 3) calculate the scores according to the fitted BUM and fdr=0.01
#' # using "pdf" method
#' scores <- dBUMscore(fit, method="pdf", fdr=0.05, scatter.bum=FALSE)
#' names(scores) <- as.character(1:length(scores))
#'
#' # 4) generate a random graph according to the ER model
#' g <- erdos.renyi.game(1000, 1/100)
#'
#' # 5) produce the induced subgraph only based on the nodes in query
#' subg <- dNetInduce(g, V(g), knn=0)
#'
#' # 6) find the module with the maximum score
#' module <- dNetFind(subg, scores)
#'
#' # 7) find the module and test its signficance
#' comm <- walktrap.community(module, modularity=TRUE)
#' significance <- dCommSignif(module, comm)

dCommSignif <- function(g, comm)
{

    if(class(g)=="graphNEL"){
        ig <- igraph.from.graphNEL(g)
    }else{
        ig <- g
    }
    if (class(ig) != "igraph"){
        stop("The function must apply to either 'igraph' or 'graphNEL' object.\n")
    }
    if(is.null(V(ig)$name)){
        V(ig)$name <- as.character(V(ig))
    }

    if (class(comm) != "communities"){
        stop("The function must apply to 'communities' object.\n")
    }


    # a function to test community significance
    community.significance.test <- function(g, vids, ...) {
        subg <- igraph::induced.subgraph(g, vids)
        within.degrees <- igraph::degree(subg)
        cross.degrees <- igraph::degree(g, vids) - within.degrees
        wilcox.test(within.degrees, cross.degrees, ...)
    }

    significance <- sapply(1:length(comm), function(x) {
        tmp <- suppressWarnings(community.significance.test(ig, vids=V(ig)$name[comm$membership==x]))
        signif(tmp$p.value, digits=3)
    })

    return(significance)
}

# this is to plot distribution of number of edges nodes have, if it shows sparsity,
# then it means that most of the graph is made of disjoint nodes.
plotDegDist <- function (ingrahObject) {
  plot(degree.distribution(ingrahObject), xlab="node degree")
  lines(degree.distribution(ingrahObject))
}



##################################################################################################
# Graph functions
##################################################################################################
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}

# create nodes
GraphNodeListCreate <- function(ingraph, mode='all') {
    nodes_ <- melt(ingraph, id.vars=c())
    nodes_ <- data.frame(value = nodes_[!((nodes_$value=="") | is.na(nodes_$value)) , ]$value)
    nodes_ <- unique(nodes_)
        return (nodes_)
}

# create edges
GraphEdgeListCreate <- function(ingraph2, id.vars) {
    edata <- melt (ingraph2, id.vars=id.vars)
    clean_ <- unique(na.omit(blankToNA(edata)))
    clean_ <- as.data.frame(sapply(clean_, trim))
    colnames(clean_) <- c("from", "to")
     rm (edata); gc()
     return (clean_)
}

getGraphObject <- function ( ind, from_, to_ ) {
  id_vars_ <- c(from_, to_)
  edge_ <- GraphEdgeListCreate ( ind[, id_vars_], id.vars=id_vars_ )
  node_ <- GraphNodeListCreate ( ind[, id_vars_] )
  node_ <- data.frame (lapply(node_, as.character), stringsAsFactors=FALSE)
  gObject <- graph_from_data_frame(d=edge_, vertices=node_, directed=F)
   return (gObject)
}


# Check for outliers:
graphRemoveOutliers <- function (ing, visual="N", remove="N") {
  out_ <- outliers::outlier(clusters(ing)$csize, opposite = FALSE, logical = FALSE)
  # visualize
  if (toupper(visual)=="Y") {
    c_size <- clusters(ing)$csize
    plot(c_size)
    qplot(seq_along(c_size), c_size)
  }

  ifrm(c_size); gc()
  cat ("|--- NOTE: Outlier found in this graph object has vertices -- ", out_, ")", "=", floor(out_), "----|", "\n")
  cl <- clusters(ing)

  # delete common vertices of outlier cluster size
  if (toupper(remove)=="Y") {
      # loop through to extract common vertices
      common_vert <- lapply(seq_along(cl$csize)[cl$csize == out_],
                        function(x) V(ing)$name[cl$membership %in% x])
      ifrm (out_)
      outg <- delete.vertices (ing, unlist(common_vert))
      return (outg)
    }
   else { return (ing) }

}

# Check for outliers:
graphRemoveOutliers.2 <- function (ing, visual="N", remove, out_) {
  out_ <- out_
  # visualize
  if (toupper(visual)=="Y") {
    c_size <- clusters(ing)$csize
    plot(c_size)
    qplot(seq_along(c_size), c_size)
  }

  ifrm(c_size); gc()
  cat (" |--- NOTE: Outlier found in this graph object has vertices >= ", "floor(", out_, ")", "=", floor(out_), "----|", "\n")
  cl <- clusters(ing)

  # delete common vertices of outlier cluster size
  if (toupper(remove)=="Y") {
      # loop through to extract common vertices
      common_vert <- lapply(seq_along(cl$csize)[cl$csize >= out_],
                        function(x) V(ing)$name[cl$membership %in% x])
      ifrm (out_)
      outg <- delete.vertices (ing, unlist(common_vert))
      return (outg)
    }
   else { return (ing) }

}


# Reference - igraph wikidot
# 1. Clique percolation
# Clique percolation is a community detection method.
# This algorithm is not implemented in igraph, but here is a quick (and rather inefficient) version to do it:
clique.community <- function(graph, k) {
   clq <- cliques(graph, min=k, max=k)
   edges <- c()
   for (i in seq_along(clq)) {
     for (j in seq_along(clq)) {
       if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
         edges <- c(edges, c(i,j)-1)
       }
     }
   }
   clq.graph <- simplify(graph(edges))
   V(clq.graph)$name <- seq_len(vcount(clq.graph))
   comps <- decompose.graph(clq.graph)

   lapply(comps, function(x) {
     unique(unlist(clq[ V(x)$name ]))
   })
}


# 2. Label propagation algorithm by Raghavan et al.
# Usha Nandini Raghavan, Réka Albert and Soundar Kumara. 2007. Near linear time algorithm to detect
# community structures in large-scale networks, Phys. Rev. E 76, 036106 Arxiv
largeScaleCommunity <- function(g,mode="all"){
  V(g)$group <- as.character(V(g))
  thisOrder <- sample(vcount(g),vcount(g))-1
  t <- 0
  done <- FALSE
  while(!done){
    t <- t+1
    cat("\rtick:",t)
    done <- TRUE ## change to FALSE whenever a node changes groups
    for(i in thisOrder){
      ## get the neighbor group frequencies:
      groupFreq <- table(V(g)[neighbors(g,i,mode=mode)]$group)
      ## pick one of the most frequent:
      newGroup <- sample(names(groupFreq) [groupFreq==max(groupFreq)],1)
      if(done){done <- newGroup==V(g)[i]$group}
      V(g)[i]$group <- newGroup
    }
  }
  ## now fix any distinct groups with same labels:
  for(i in unique(V(g)$group)){
    ## only bother for connected groups
    if(!is.connected(subgraph(g,V(g)[group==i]))){
      theseNodes <- V(g)[group==i]
      theseClusters <- clusters(subgraph(g,theseNodes))
      ## iterate through the clusters and append their names
      for(j in unique(theseClusters$membership)){
        V(g)[theseNodes[theseClusters$membership==j]]$group <- paste(i,j,sep=".")
      }
    }
  }
  return(g)
}


# Testing the significance of a community
# The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Call the edges within a community "internal"
# and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external"
# edges incident to a vertex of the community. More internal than external edges show that the community
# is significant; less internal than external edges show that the community is in fact an "anti-community".
# The p-value of the test performed by this function will be close to zero in both cases; the value of the
# test statistic tells us whether we have a community or an anti-community.
community.significance.test <- function(graph, vs, ...) {
    if (is.directed(graph)) stop("This method requires an undirected graph")
    subgraph <- induced.subgraph(graph, vs)
    in.degrees <- degree(subgraph)
    out.degrees <- degree(graph, vs) - in.degrees
    wilcox.test(in.degrees, out.degrees, ...)
}

# USE CASE
#' # 1) generate an vector consisting of random values from beta distribution
#' x <- rbeta(1000, shape1=0.5, shape2=1)
#'
#' # 2) fit a p-value distribution under beta-uniform mixture model
#' fit <- dBUMfit(x, ntry=1, hist.bum=FALSE, contour.bum=FALSE)
#'
#' # 3) calculate the scores according to the fitted BUM and fdr=0.01
#' # using "pdf" method
#' scores <- dBUMscore(fit, method="pdf", fdr=0.05, scatter.bum=FALSE)
#' names(scores) <- as.character(1:length(scores))
#'
#' # 4) generate a random graph according to the ER model
#' g <- erdos.renyi.game(1000, 1/100)
#'
#' # 5) produce the induced subgraph only based on the nodes in query
#' subg <- dNetInduce(g, V(g), knn=0)
#'
#' # 6) find the module with the maximum score
#' module <- dNetFind(subg, scores)
#'
#' # 7) find the module and test its signficance
#' comm <- walktrap.community(module, modularity=TRUE)
#' significance <- dCommSignif(module, comm)

dCommSignif <- function(g, comm)
{

    if(class(g)=="graphNEL"){
        ig <- igraph.from.graphNEL(g)
    }else{
        ig <- g
    }
    if (class(ig) != "igraph"){
        stop("The function must apply to either 'igraph' or 'graphNEL' object.\n")
    }
    if(is.null(V(ig)$name)){
        V(ig)$name <- as.character(V(ig))
    }

    if (class(comm) != "communities"){
        stop("The function must apply to 'communities' object.\n")
    }


    # a function to test community significance
    community.significance.test <- function(g, vids, ...) {
        subg <- igraph::induced.subgraph(g, vids)
        within.degrees <- igraph::degree(subg)
        cross.degrees <- igraph::degree(g, vids) - within.degrees
        wilcox.test(within.degrees, cross.degrees, ...)
    }

    significance <- sapply(1:length(comm), function(x) {
        tmp <- suppressWarnings(community.significance.test(ig, vids=V(ig)$name[comm$membership==x]))
        signif(tmp$p.value, digits=3)
    })

    return(significance)
}

# this is to plot distribution of number of edges nodes have, if it shows sparsity,
# then it means that most of the graph is made of disjoint nodes.
plotDegDist <- function (ingrahObject) {
  plot(degree.distribution(ingrahObject), xlab="node degree")
  lines(degree.distribution(ingrahObject))
}