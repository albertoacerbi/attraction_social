library(tidyverse)
library(tictoc)
library(matrixStats)



rescale <- function(original, new_min, new_max){
  current_max = max(original)
  current_min = min(original)
  output =((original - current_min)*(new_max - new_min))/(current_max - current_min) + new_min
  output
}


plot_multiple_runs <- function(data_model) {
  ggplot(data = data_model, aes(y = p, x = generation)) +
    geom_line(aes(group = run), colour = "coral") +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = "p")
}

# ACTUAL SIMS:


# discrete - from Acerbi, Mesoudi, Smolla, IBM book:
biased_transmission_demonstrator_discrete <- function(N, p_0, c_s, c_low, alpha_attr, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, 
                                        replace = TRUE, prob = c(p_0, 1 - p_0)),
                         status = sample(c("high", "low"), N,
                                         replace = TRUE, prob = c(c_s, 1 - c_s))) 
    
    # Assign copying probabilities based on individuals' status
    p_demonstrator <- rep(1,N)
    p_demonstrator[population$status == "low"] <- c_low
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    
    for (t in 2:t_max) {
      
      # Create a new population that would update using prestige:
      prestige_population <- population
      
      # Copy traits based on status
      if(sum(p_demonstrator) > 0){
        demonstrator_index <- sample (N, prob = p_demonstrator, replace = TRUE)
        prestige_population$trait <- population$trait[demonstrator_index]
      }
      
      # Copy back in population:
      population <- prestige_population 
      
      # Attraction:
      change_ind <- sample(c("TRUE", "FALSE"), N, prob = c(alpha_attr, 1-alpha_attr), replace = TRUE)
      if(sum(change_ind == TRUE) > 0)
        population$trait[change_ind == TRUE] <- "A"
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
    
  }
  # Export data from function
  output 
}



# TESTS:
# tic()
# data_model <- conformist_transmission_continuous(N = 1000, p_0 = 0, alpha_attr = 0.01, t_max = 100, r_max = 10)
# plot_multiple_runs(data_model)
# # in the continuous case, no matter how small is alpha, at some point it will always stabilise on the attractor! 
# toc()
# 
tic()
data_model <- biased_transmission_demonstrator_discrete(N = 1000, p_0 = 0.5, c_s = 0.01, c_low = 0.001, alpha_attr = 0.1, t_max = 100, r_max = 100)
plot_multiple_runs(data_model)
toc()



