library(OUwie)
library(tidyverse)


#data to be fed into OUwie, change variables as needed
mods <- c("BM1", "BMS", "OU1", "OUM", "OUMA", "OUMV", "OUMVA") #set of models to run
simtree <- make.simmap(phy, disc_trait, nsim=100) # make simmaps
dt <- data.frame(phy$tip.label, disc_trait, cont_trait) #create OUwie data frame


#OUwie func with attributes pre-set, change OUwie attributes if needed
myOUwie <- function(tree, model, data){ 
  OUwie(tree, data, model=model, simmap.tree=TRUE, root.station=TRUE, diagn=T)
}


#runs OUwie analysis on each combination of tree and model,
#only run once (will take a while depending on models run/num of simmaps)
raw_output <- tibble(tree_id = seq_along(simtree), tree = simtree) %>%
  crossing(model = mods) %>%  #make each combination of tree and model
  mutate(res = pmap(list(tree, model), ~ myOUwie(.x, .y, dt))) #apply each combo of model/tree to OUwie


#extracts individual variables from OUwie output
df <- raw_output %>% mutate(aicc = map_dbl(res, "AICc"), 
                            lnl = map_dbl(res, "loglik"), 
                            eigval = map(res, "eigval"), 
                            theta = map(res, ~.$theta[,1]), 
                            alpha = map(res, ~.$solution.se[1,]), 
                            sigma.sq = map(res, ~.$solution.se[2,]))


# data frame with only variables of interest
df %>% select(., -c(tree,res))
