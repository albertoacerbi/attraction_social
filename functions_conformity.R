library(tidyverse)
library(tictoc)
library(matrixStats)


plot_multiple_runs <- function(data_model) {
  ggplot(data = data_model, aes(y = p, x = generation)) +
    geom_line(aes(group = run), colour = "coral") +
    stat_summary(fun = mean, geom = "line", size = 1) +
    scale_y_continuous(breaks=seq(0,1,.25), limits = c(0, 1.1)) +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = "p")
}

# ACTUAL SIMS:


# discrete - from Acerbi, Mesoudi, Smolla, IBM book + attraction (biased mutation in the discrete case):
conformist_transmission_discrete <- function (N, p_0, D, alpha_attr, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE, prob = c(p_0, 1 - p_0)))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- sum(population$trait == "A") / N 
    
    for (t in 2:t_max) {
      
      # Create a tibble with a set of 3 randomly-picked demonstrators for each agent
      demonstrators <- tibble(dem1 = sample(population$trait, N, replace = TRUE), 
                              dem2 = sample(population$trait, N, replace = TRUE), 
                              dem3 = sample(population$trait, N, replace = TRUE))
      
      # Get the number of As in each 3-demonstrator combinations
      num_As <- rowSums(demonstrators == "A")
      
      # Create a new population that would update using conformity:
      conf_population <- population
      
      # For 3-demonstrator combinations with all As, set to A
      conf_population$trait[num_As == 3] <- "A"  
      # For 3-demonstrator combinations with all Bs, set to B
      conf_population$trait[num_As == 0] <- "B"  
      
      prob_majority <- sample(c(TRUE, FALSE), prob = c((2/3 + D/3), 1 - (2/3 + D/3)), N, replace = TRUE)
      prob_minority <- sample(c(TRUE, FALSE), prob = c((1/3 - D/3), 1 - (1/3 - D/3)), N, replace = TRUE)
      
      # 3-demonstrator combinations with two As and one B
      if (nrow(population[prob_majority & num_As == 2, ]) > 0) {
        conf_population[prob_majority & num_As == 2, ]$trait <- "A"
      }
      if (nrow(population[prob_majority == FALSE & num_As == 2, ]) > 0) {
        conf_population[prob_majority == FALSE & num_As == 2, ]$trait <- "B"
      }  
      # 3-demonstrator combinations with one A and two Bs
      if (nrow(population[prob_minority & num_As == 1, ]) > 0) {
        conf_population[prob_minority & num_As == 1, ]$trait <- "A"
      }
      if (nrow(population[prob_minority == FALSE & num_As == 1, ]) > 0) {
        conf_population[prob_minority == FALSE & num_As == 1, ]$trait <- "B"
      }  
      
      # Copy back in population:
      population <- conf_population 
      
      # Attraction:
      change_ind <- sample(c("TRUE", "FALSE"), N, prob = c(alpha_attr, 1-alpha_attr), replace = TRUE)
      if(sum(change_ind == TRUE) > 0)
        population$trait[change_ind == TRUE] <- "A"
      
      # Get p and put it into output slot for this generation t and run r:
      output[output$generation == t & output$run == r, ]$p <- sum(population$trait == "A") / N 
    }
  }
  # Export data from function:
  output  
}


# continuous - conformity as copying pop mean, from Morgan & Thompson 2022

conformist_transmission_continuous <- function (N, alpha_attr, beta, sd_error, p_0, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   sd = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  for (r in 1:r_max) {
    if(is.na(p_0)){
    # around the central value: (also consider a beta distribution)
      population <- rnorm(N, .5, 1)  
    }
    else{
    # similar to the discrete case:
     population <- sample(c(1, 0), N, replace = TRUE, prob = c(p_0, 1 - p_0))
    }
    
    for(t in 1:t_max){
      
      demonstrators = cbind(sample(population, N, replace = TRUE), 
                            sample(population, N, replace = TRUE), 
                            sample(population, N, replace = TRUE))
      
      conf_population <- population
      
      errors <- rnorm(N, 0, sd_error) #
      conf_population <- (1 - beta)*population + beta*rowMeans(demonstrators) + 
                         (1 - population)*alpha_attr + errors
    
      
      # Copy back in population:
      population <- conf_population 

      # Get p and put it into output slot for this generation t and run r:
      output[output$generation == t & output$run == r, ]$p <- mean(population)
      output[output$generation == t & output$run == r, ]$sd <- sd(population)
      
    }
  }
  # Export data from function:
  output  
}



# TESTS HERE:
# tic()
# data_model <- conformist_transmission_continuous(N = 1000, p_0 = NA, alpha_attr = .1, t_max = 100, r_max = 10, beta = .5,
#                                                    sd_error = .1)
# plot_multiple_runs(data_model)
#  # in the continuous case, no matter how small is alpha, at some point it will always stabilise on the attractor!
# toc()
# # # 
# tic()
# data_model <- conformist_transmission_discrete(N = 1000, p_0 = 0.5, D = 1, alpha_attr = 0.1, t_max = 50, r_max = 10)
# plot_multiple_runs(data_model)
# toc()



