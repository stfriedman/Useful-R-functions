library(rfishbase)
library(tidyverse)
library(ape)


# Function takes a fish phylogeny, grabs taxonomic data from fishbase and prunes
# tree to the designated clade level. Polyphyletic groupings are preserved but noted. 
# Clade_level can be Genus, Family, Order, or Class and the funciton assumes the tree has
# tips with Genus_species.

collapse_clades <- function(tree, clade_level){
  taxa <- load_taxa() %>% 
    # pull taxonomy data from fishbase
    select(Genus, Species, Family, Order, Class) %>%
    mutate(Genus_species = paste0(Genus, "_", Species)) %>%
    # keep only species in phylogeny
    right_join(., tibble(
      Genus_species=as.character(tree$tip.label)), by="Genus_species") %>%
    # arrange species to match order of tips on tree
    arrange(match(Genus_species, tree$tip.label))  %>%
    mutate_(new_level = clade_level)
  
  if(any(is.na(taxa$new_level))){
    cat("Taxonomic information for some species could not be pulled from fishbase.\n")
    ans <- readline("Do you wish to see a species list? (y/n)")
    if (substr(ans, 1, 1) == "y")
    print(taxa %>% filter(is.na(new_level)) %>% .$Genus_species)
  }
  
  newtree <- ladderize(tree)
  newtree$tip.label <- taxa$new_level
  
  check_monophyly <- taxa %>% 
    select(new_level) %>% distinct() %>%
    mutate(m = map_lgl(new_level,
                       ~is.monophyletic(newtree, tips=which(newtree$tip.label == .x)))) 

  
  if(all(check_monophyly$m) == FALSE) {
    cat("Be careful! Not all groups are monophyletic. \nCheck the following clades:\n")
    print(check_monophyly %>% filter(m == FALSE & !is.na(new_level)) %>% .$new_level)
  }
  
  new_tips <- taxa %>% select(new_level) %>%
    mutate(dups = new_level != dplyr::lag(new_level, default="yes"),
           new_tips = ifelse(dups != TRUE | is.na(dups), "to_remove", new_level)) 
    
  outtree <- newtree
  outtree$tip.label <- new_tips$new_tips
  outtree <- drop.tip(outtree, "to_remove")
  invisible(outtree)
}





# ------------- EXAMPLE ------------- #

# pulling species names from fishbase
spp <- gsub(" ", "_", species_list(Family = "Acanthuridae"))

# simulating tree with species as tips
tr <- pbtree(n = length(spp))
tr$tip.label <- spp

plot(collapse_clades(tr, "Genus"), cex=0.8)
