# Function to plot Monte Carlo simulation distributions based on Erdos-Renyi Random Graphs

# Package dependencies
library(igraph)
library(cowplot)
library(ggplot2)

graph_mc_sim <- function(graph, sim = 5000){
  
  # Check that graph is an igraph object
  if (!is_igraph(graph)) {
    stop("Not a graph object")
  }
  
  # Check that graph is directed 
  if(!is_directed(graph)){
    stop("Graph is not directed")
  }
  
  # Prompt user for input on name of graph
  g_name <- readline(prompt = "What name would you like to use for the graph in the plots?: ")
  g_name <- as.character(g_name)
  
  # Initiate empty list for housing transitivity simulations
  gl <- vector('list', sim)
  
  # Initiate empty list for housing density, average path length, and mean degree simulations
  gl.d <- vector('list', sim)
  
  # Populate list with random graphs of same order and size
  for(i in 1:sim){
    gl[[i]] <- erdos.renyi.game(n = gorder(graph), p.or.m = gsize(graph), directed = TRUE, type = "gnm")
  }
  
  # Populate gl.d list with random graphs of same order and approximate density. 
  # A separate list of randon graphs is necessary for density, average path length, and 
  # mean degree because these statistics would be identical in random graphs of the same
  # order and size as the observed graph. 
  # Instead, a probability of edge creation equal to the observed density is used. 
  # Further, only mean degree (as opposed to mean weighted degree) is used because 
  # Erdos-Renyi random graphs do not support weights. 
  for(i in 1:sim){
    gl.d[[i]] <- erdos.renyi.game(n = gorder(graph), p.or.m = edge_density(graph), directed = TRUE, type = "gnp")
  }
  
  # Calculate average path length, transitivity (custering coefficient), density, and 
  # degree across the random graphs
  gl.pl <- lapply(gl.d, mean_distance, directed = TRUE)
  gl.trans <- lapply(gl, transitivity)
  gl.density <- lapply(gl.d, edge_density)
  gl.degree <- lapply(gl.d, function(x){
    y <- degree(x)
    mean(y)
  }
  )
  
  # Unlist and change to a data frame for vizualizations
  gl.pl <- as_tibble(unlist(gl.pl))
  gl.trans <- as_tibble(unlist(gl.trans))
  gl.density <- as_tibble(unlist(gl.density))
  gl.degree <- as_tibble(unlist(gl.degree))
  
  # Plot the distribution of random graph's average shortest path lengths with the 
  # input graphs's ave. shortest path as line
  p.gl.pl <- ggplot(gl.pl, aes(x = value)) + 
    geom_histogram(aes(y = ..density..)) + 
    geom_vline(xintercept = (mean_distance(graph, directed = TRUE)), linetype = "dashed", color = "red") +
    geom_density() +
    ggtitle(paste0("Distribution of ", sim, " Random Graph Average Shortest Path Lengths & \n Observed Average Shortest Path Length in ", g_name)) + 
    xlab("Average Shortest Path Length") +
    ylab("")
  
  # Plot the distribution of random graph's transitivity with the input graph's 
  # transitivity path as line
  p.gl.trans <- ggplot(gl.trans, aes(x = value)) + 
    geom_histogram(aes(y = ..density..)) + 
    geom_vline(xintercept = (transitivity(graph)), linetype = "dashed", color = "red") +
    geom_density() +
    ggtitle(paste0("Distribution of Transitivity in ", sim, " Random Models & \n Observed Transitivity in ", g_name)) + 
    xlab("Transitivity (or Clustering Coefficient)") +
    ylab("")
  
  # Plot the distribution of random graph's average density with the input graph's 
  # denisty as line
  p.gl.density <- ggplot(gl.density, aes(x = value)) + 
    geom_histogram(aes(y = ..density..)) + 
    geom_vline(xintercept = (edge_density(graph)), linetype = "dashed", color = "red") +
    geom_density() +
    ggtitle(paste0("Distribution of ", sim,  " Random Graph Average Densities &\n Observed Average Density in ", g_name)) + 
    xlab("Average Density") +
    ylab("")
  
  # Plot the distribution of random graph's mean degree with the input graph's mean 
  # degree path as line
  p.gl.degree <- ggplot(gl.degree, aes(x = value)) + 
    geom_histogram(aes(y = ..density..), bins = 10) + 
    geom_vline(xintercept = (mean(degree(graph, mode = "all"))), linetype = "dashed", color = "red") +
    geom_density() +
    ggtitle(paste0("Distribution of Mean Degree in ", sim, " Random Models & \n Observed Mean Degree in ", g_name)) + 
    xlab("Mean Degree") +
    ylab("")
  
  # Use plot_grid to plot all four graphs in the same grid
  plot_grid(p.gl.pl, p.gl.trans, p.gl.density, p.gl.degree)
}