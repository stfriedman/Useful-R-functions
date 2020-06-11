get_famtree <- function(tree, family, plot){
  fm <- family
  # pull all hawkfish species listed in Fishbase
  spp <- gsub(" ", "_", species_list(Family = fm)) 
  
  # prune phylogeny to only hawkfish species
  spptree <- drop.tip(tree, setdiff(tree$tip.label, spp)) 
  
  # plot tree
  if(hasArg(plot)){
    if(plot == TRUE){
  par(mar=c(3,0,0,0))
  plot(ladderize(spptree), cex=0.6)
  axisPhylo()
    }
  }
  
  # MRCA node for fam
  fm_mrca <- getMRCA(tree, spp[spp %in% spptree$tip.label])
  
  # Age of family
  mrca <- branching.times(tree)[as.character(fm_mrca)]
  invisible(list(mrca=mrca, famtree = spptree))
}

get_famtree(tree, "Scombridae", plot=TRUE)
get_famtree(tree, "Chaetodontidae", plot=TRUE)
