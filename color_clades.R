library(viridis)

color_clades <- function(tree, nodes) {
  n <- length(nodes) 
  cols <- viridis(n)
  br_col <- rep("grey85", nrow(tree$edge))
  
  for(i in 1:n){
    clade_desc <- getDescendants(tree, nodes[[i]])
   
     if(all(!clade_desc %in% nodes)){
      br_col[which.edge(tree, clade_desc)] <- cols[[i]]
    } else {
      overlap_clades <- clade_desc[which(clade_desc %in% nodes)]
      
      if(length(overlap_clades) > 1){
        clade_edges <- c()
        for(j in 1:length(overlap_clades)) {
          clade_edges <- append(clade_edges, 
                                which.edge(tree, getDescendants(tree, overlap_clades[[j]])))
        }
        
      } else {
      clade_edges <- which.edge(tree, getDescendants(tree,overlap_clades))
      } 
      
      overlap_edges <- which.edge(tree, clade_desc)
      br_col[overlap_edges[!overlap_edges %in% clade_edges]] <- cols[[i]]
    }
  }
  
  clade_cols <- br_col
  return(clade_cols)
}


# EXAMPLE
tree <- pbtree(n=50)
nodes <- c(52, 69, 85, 96, 76, 74)
n <- length(nodes)

edge.color <- color_clades(tree, nodes, n)

plot(tree, edge.color = edge.color, type="fan", edge.width = 1.5, label.offset = 0.1)
nodelabels(node=nodes,  col="red", cex=1)

