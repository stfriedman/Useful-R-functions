# Function will estimate PICs and ancestral states at each node in a simmap, as well as identify nodes that are associated with particular discrete state transitions.

### input interpretation: 
# Data is a dataframe containing tip labels and a single continuous trait in columns (column order is unimportant) and tree is either a phylo or multiPhylo object that can have mapped states. If trees are not simmaps, include tip_states argument (a named vector with tip states) to run make.simmap internally, otherwise exclude tip_states argument. The ellipses allow you to add in make.simmap arguments (see documentation for make.simmap). If anc = TRUE, function will estimate both the ancestral state and pic at each node in each tree for the provided continuous trait. Otherwise, function returns a tibble with each node in each tree (if multiPhylo), the estimated discrete state of that node (state) and whether or not it is followed by a transition (node_type), as well as the direction of the state transition. Ouitput also includes a column (edge) that tells which node descendent branch (left vs. right) the transition occurs on. 

pic_test <- function(data, tree, tip_states, anc = TRUE, ..){
  map <- purrr:::map
  select <- dplyr:::select
  
  tips <- colnames(select_if(data, function(col) is.factor(col) | is.character(col))) 
  trait_data <- data %>% select(-tips) %>% colnames()
  
  #checking to make sure data and tree in same order and re-ordering data if not
  if(!all(data[[tips]] == tree$tip.label)) {
    data <- data %>%
      filter_(.[[tips]] %in% tree$tip.label) %>%
      arrange(match(.[[tips]], tree$tip.label))
  }
  if(!all(data[[tips]] == tree$tip.label)) stop("phylogeny and data are not in the same order")
  
  #make simmaps if phylogeny doesn't have mapped states
  if(!any(class(tree) %in% c("simmap", "multiSimmap"))){
    if(!hasArg(tip_states)) stop("need to include argument tip_states to make simmaps")
    tree <- make.simmap(tree, tip_states, ...)
  }
  
  # function to get the mapped edge from each simmap
  get_mapped_edge <- function(x){
    data.frame(x$mapped.edge) %>%
      rownames_to_column("edges") %>%
      as_tibble() %>%
      separate(edges, c("node_1", "node_2"), sep = ",")
  }
  
  
  if(any(class(tree) == "multiPhylo")){
    n <- length(tree)
    tree <- structure(tree, class = c("list","multiSimmap","multiPhylo"))
    # getting mapped edge from each simmap
    sim_tib <- tibble(sim_id = 1:n) %>%
      mutate(simmap = tree,
             mapped.edge = purrr:::map(simmap, get_mapped_edge)) 
  } else {
    n <- 1
    # getting mapped edge from each simmap
    sim_tib <- tibble(sim_id = 1:n) %>%
      mutate(simmap = list(tree),
             mapped.edge = purrr:::map(simmap, get_mapped_edge)) 
  }
  
  get_node_state <- function(x){
    y <- sapply(x$maps,function(x) names(x)[1])
    names(y) <- x$edge[,1]
    y <- y[as.character(length(x$tip) + 1:x$Nnode)]
    tibble(node = names(y), state = y)
  }
  
  
  #classifying nodes as transition or not based on occurance (i.e. occurs > 1)
  node_states <- sim_tib %>% 
    mutate(state = map(simmap, get_node_state)) %>%
    select(sim_id, state) %>%
    unnest()
    
  node_df <- sim_tib %>%
    select(sim_id, mapped.edge) %>%
    unnest() %>%
    group_by(sim_id, node_1) %>%
    mutate(edge = row_number(),
           edge = ifelse(edge == 1, "right", "left")) %>%
    gather(state, prob, -node_1, -node_2, -sim_id, -edge) %>%
    filter(prob != 0) %>%
    group_by(sim_id, node_1, node_2) %>%
    add_count() %>%
    mutate(node_type = ifelse(n > 1, "transition", state))
  
  # data frame with all nodes
  all_nodes <- node_df %>%
    filter(node_type == "transition") %>%
    ungroup() %>%
    select(-prob) %>%
    arrange(sim_id, node_1, node_2) %>%
    right_join(node_states, by = c("sim_id", "node_1" = "node")) %>%
    filter(state.x != state.y | is.na(state.x)) %>%
    mutate(state = ifelse(is.na(node_type), state.y, 
                           paste0(state.y, "_to_", state.x)),
           node_type = ifelse(is.na(node_type), "same", "transition")) %>% 
    group_by(sim_id, state) %>%
    add_count() %>%
    select(sim_id, node_1, n, edge, node_type, state)
 
  if(anc == TRUE){
  # combines transition nodes with their associated PICs into a complete df
  if(any(class(tree) == "multiPhylo")){
    pics <- pic(data %>% select(trait_data) %>% .[[trait_data]], tree[[1]])
    v <- setNames(data[[trait_data]], data[[tips]])
    aces <- fastAnc(tree[[1]], v)
  } else {
    pics <- pic(data %>% select(trait_data) %>% .[[trait_data]], tree)
    v <- setNames(data[[trait_data]], data[[tips]])
    aces <- fastAnc(tree, v)
  }
  
  stopifnot(all(names(pics) == names(aces))) 
    
    pic_df <- data.frame(node_1 = names(pics), pic = pics, ace = as.vector(aces)) %>%
      mutate(node_1 = as.character(node_1))
    
    full_df <- all_nodes %>%
      inner_join(pic_df, by = "node_1") 
    return(full_df)
    
  } else {
    return(all_nodes)
  }
  
}



# # # ------------------------- EXAMPLE ----------------------------------------------
# n <- 30
# tr <- ladderize(pbtree(n = n))
# reg <- c("a", "b", "c")
# data <- cbind.data.frame(tips = tr$tip.label, x = rnorm(n, 0, 1))
# states <- setNames(rep(reg, each = n/length(reg)), tr$tip.label)
# tree <- make.simmap(tr, states, nsim = 5)
# 
# 
# #checking to make sure function identifies proper nodes
# test <- pic_test(data, tree)
# # plotting just transition nodes from the first simmap
# a <- test %>% filter(node_type == "transition" & sim_id == 1)
# plot(tree[[1]])
# nodelabels(node = as.numeric(a$node_1), pch = 16, cex = 2, col = "black")
