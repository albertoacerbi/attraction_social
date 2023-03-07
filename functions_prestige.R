library(tidyverse)
library(tictoc)
library(RColorBrewer) # with colour-blind-friendly palettes


# softmax 
softmax <- function(x, temp = 1) {
  exp(x / temp) / sum(exp(x / temp))
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

rescale <- function(original, new_min, new_max){
  current_max = max(original)
  current_min = min(original)
  output =((original - current_min)*(new_max - new_min))/(current_max - current_min) + new_min
  output
}

# ACTUAL SIMS:

# discrete - from Acerbi, Mesoudi, Smolla, IBM book + JW's softmax function + attraction (biased mutation in the discrete case):
biased_transmission_demonstrator_discrete <- function(N, p_0, c_s, c_copy, alpha_attr, t_max, r_max) {
  
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
    p_demonstrator[population$status == "high"] <- 1 + c_copy
    p_demonstrator <- softmax(p_demonstrator)
    
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

## ----
# continuous: 

biased_transmission_demonstrator_continuous <- function(N, p_0, c_s, c_copy, alpha_attr, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    if(is.na(p_0)){
      population <- tibble(trait = rescale(rnorm(N),0,1),
                           status = sample(c("high", "low"), N,
                                           replace = TRUE, prob = c(c_s, 1 - c_s))) 
    }
    else{
      population <- tibble(trait = sample(c(1, 0), N, 
                                          replace = TRUE, prob = c(p_0, 1 - p_0)),
                          status = sample(c("high", "low"), N,
                                         replace = TRUE, prob = c(c_s, 1 - c_s))) 
    }
    
    # Assign copying probabilities based on individuals' status
    p_demonstrator <- rep(1,N)
    p_demonstrator[population$status == "high"] <- 1 + c_copy
    p_demonstrator <- softmax(p_demonstrator)
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- mean(population$trait)
    
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
      if(sum(change_ind == TRUE) > 0){
        changes <- runif(sum(change_ind == TRUE), 0, 1 - population[change_ind == TRUE,]$trait )
        population[change_ind == TRUE,]$trait <- population[change_ind == TRUE,]$trait + changes
      }
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- mean(population$trait)
    }
    
  }
  # Export data from function
  output 
}




# TESTS HERE:

# tic()
# data_model <- biased_transmission_demonstrator_discrete(N = 1000, p_0 = 0.5, c_s = 0.01, c_copy = 1, alpha_attr = 0, t_max = 200, r_max = 10)
# plot_multiple_runs(data_model)
# toc()
# 
# tic()
# data_model <- biased_transmission_demonstrator_continuous(N = 1000, p_0 = NA, c_s = 0.01, c_copy = 1, alpha_attr = 0, t_max = 200, r_max = 10)
# plot_multiple_runs(data_model)
# toc()
# # 
