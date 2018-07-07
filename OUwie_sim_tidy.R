library(OUwie)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(phytools)


# ------- Change user-defined variables ------ #
phy <- phy # your phylogeny
disc_trait <- disc_trait # named vector matching tip labels on phylogeny
cont_trait <- cont_trait # continuous trait

# usually these parameters will be estimated from your best fit OUwie model
# these values should be changed according to model (see OUwie.sim documentation)
alpha = c(1.0,0.5)
sigma.sq = c(0.45,0.9)
theta0 = 1.0
theta = c(1.0,2.0)




# ------- Simulate data on each simmap under best fit parameters ------ #

# data to be fed into OUwie_sim, change variables as needed
simtree <- make.simmap(phy, disc_trait, nsim=50) # make simmaps, change nsim accordingly
dt <- data.frame(phy$tip.label, disc_trait, cont_trait) #create OUwie data frame



# OUwie.sim function with attributes pre-set, outputs simulated data in correct format
# to input into OUwie function
myOUwie_sim <- function(tree, alpha, sigma.sq, theta0, theta){ 
  s <- OUwie.sim(tree, simmap.tree=TRUE, 
                 alpha = alpha, sigma.sq = sigma.sq, theta0 = theta0, theta = theta)
  out <- as_tibble(s) %>%
    left_join(., data.frame(Genus_species=names(disc_trait), reg=disc_trait), by="Genus_species") %>%
    select(Genus_species, reg, X) %>% data.frame
  out
}


# runs OUwie.sim on each simmap, get simulated data under known parameters for each simmap
# ignore warning messages
sim_data <- tibble(tree_id = seq_along(simtree), simtree = simtree) %>%
  mutate(simdat = map(simtree, ~myOUwie_sim(.x, alpha, sigma.sq, theta0, theta))) 




# ------- Run OUwie on simulated data ------ #
# this allows us to determine if we have the statistical power to recover the same best fit model
# that these data were simulated under

mods <- c("BM1", "BMS", "OU1", "OUM", "OUMA", "OUMV", "OUMVA") #set of models to run

#OUwie func with attributes pre-set, change OUwie attributes if needed
myOUwie <- function(tree, data, model){ 
  OUwie(tree, data, model=model, simmap.tree=TRUE, root.station=TRUE, diagn=TRUE)
}


# this iterates each simmap and simulated data set through all possible OUwie models
sim_output <- sim_data %>%
  crossing(model = mods) %>% #make each combination of tree and model
  mutate(res = pmap(list(simtree, simdat, model), ~myOUwie(..1, ..2, ..3))) 




# ------- Cleaning up OUwie output and plotting parameter distributions ------ #

ouwie_results <- sim_output %>% 
  mutate(aicc = map_dbl(res, "AICc"), 
       lnl = map_dbl(res, "loglik"), 
       eigval = map(res, "eigval"), 
       theta = map(res, ~.$theta[,1]), 
       alpha = map(res, ~.$solution.se[1,]), 
       sigma.sq = map(res, ~.$solution.se[2,])) %>%
  select(-simtree, -res) %>%
  filter(map_lgl(eigval, ~all(.x > 0))) #filters out runs on saddlepoints (unreliable results)


ggplot(ouwie_results, aes(aicc, group=model, col=model)) +
  geom_density(alpha = 0.7, size=1) +
  theme_classic() +
  ggtitle("AICc") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_color_manual(values = viridis(7))


# plots estimated theta, alpha, and sigma.sq for all OUwie runs, dashed lines represent
# parameter value simulated under
reg_cols <- c("goldenrod1", "deepskyblue3")
ouwie_results$model <- factor(ouwie_results$model, rev(mods))


ouwie_results %>%
  mutate(theta = map(theta, ~ tibble(theta=.x, regime=as.character(unique(disc_trait))))) %>%
  unnest(theta, alpha, sigma.sq) %>%
  gather(key = "param", value="value", alpha, theta, sigma.sq) %>%
  filter(value > 0) %>% 
  
  ggplot(., aes(fill=regime)) +
  geom_density(aes(value), col="grey30", alpha=0.6) +
  facet_grid(model ~ param, switch="both", scales="free") +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(0, "lines"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle = 180, size=10),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
    ) +
  scale_fill_manual(values = reg_cols) +
  geom_vline(data=tibble(alpha, col=reg_cols, param="alpha"), 
             aes(xintercept = alpha, col=col), lty=2) +
  geom_vline(data=tibble(sigma.sq, col=reg_cols, param="sigma.sq"), 
             aes(xintercept = sigma.sq, col=col), lty=2) +
  geom_vline(data=tibble(theta, col=reg_cols, param="theta"), 
             aes(xintercept = theta, col=col), lty=2) +
  scale_colour_identity()
