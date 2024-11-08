# call necessary functions and libraries:

source("functions_conformity.R")


# FIGURE 1:

# FIGURE 1 A
tic()
data_model <- conformist_transmission_discrete(N = 1000, p_0 = 0.5, D = 1, alpha_attr = 0, t_max = 50, r_max = 100)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "p (proportion of individuals with trait A)")
ggsave("figures/fig1A.pdf", width = 4, height = 4)
toc()


# FIGURE 1 B
tic()
data_model <- conformist_transmission_discrete(N = 1000, p_0 = 0.5, D = 1, alpha_attr = 0.1, t_max = 50, r_max = 100)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "p (proportion of individuals with trait A)")
ggsave("figures/fig1B.pdf", width = 4, height = 4)
toc()


# conformity continuous 
# in the continuous case, no matter how small is alpha, at some point it will always stabilise on the attractor! 

# FIGURE 1 C
tic()
data_model <- conformist_transmission_continuous(N = 1000, p_0 = NA, alpha_attr = 0, t_max = 100, r_max = 100, 
                                                 beta = .5, sd_error = .1)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = expression(bar(p)))
ggsave("figures/fig1C.pdf", width = 4, height = 4)
toc()

# FIGURE 1 D
tic()
data_model <- conformist_transmission_continuous(N = 1000, p_0 = NA, alpha_attr = 0.1, t_max = 100, r_max = 100, 
                                                 beta = .5, sd_error = .1)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1.05)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = expression(bar(p)))
ggsave("figures/fig1D.pdf", width = 4, height = 4)
toc()

# inset FIGURE 1 C

data_model <- conformist_transmission_continuous(N = 1000, p_0 = NA, alpha_attr = 0, t_max = 100, r_max = 1,
                                                 beta = .5, sd_error = .1)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = p-sd, ymax=p+sd), linetype=2, alpha=0.5) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = expression(bar(p))) +
  theme(axis.text = element_text(size = 20)) +
  theme(text = element_text(size = 20))
  ggsave("figures/fig1C_inset.pdf", width = 4, height = 4)



# FIGURE 2:

# FIGURE 2 A
tic()
data_model_alpha1 <- conformist_transmission_discrete(N = 1000, p_0 = 0, D = 1, alpha_attr = 0.1, t_max = 50, r_max = 100)
data_model_alpha2 <- conformist_transmission_discrete(N = 1000, p_0 = 0, D = 1, alpha_attr = 0.2, t_max = 50, r_max = 100)

data_model_alpha1X <- data_model_alpha1 %>%
  add_column(alpha = as_factor(.1))
data_model_alpha2X <- data_model_alpha2 %>%
  add_column(alpha = as_factor(.2))

data_model <- bind_rows(data_model_alpha1X, data_model_alpha2X)


ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = interaction(run, alpha), colour = alpha)) +
  scale_colour_manual(values = c("coral", "dodgerblue")) +
  stat_summary(aes(group = alpha), fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "p (proportion of individuals with trait A)") +
  ggsave("figures/fig2A.pdf", width = 4, height = 4)

toc()


# FIGURE 2 B
tic()
data_model <- conformist_transmission_continuous(N = 1000, p_0 = 0, alpha_attr = 0.1, t_max = 100, r_max = 100)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = expression(bar(p))) +
  ggsave("figures/fig2B.pdf", width = 4, height = 4)
toc()

# FIGURE 3

# results are more complicated for the discrete case: we study relationship between p_0 and alpha:
# alpha_attractor_cycle = seq(from = .01, to = .2, by =.01)
# p_0_cycle = seq(from = 0, to = .5, by = .02)
# 
# output <- tibble(a = rep(alpha_attractor_cycle, each = length(p_0_cycle)),
#                  p = rep(p_0_cycle, length(alpha_attractor_cycle)),
#                  stabile = as.numeric(NA))
# 
# for(a in alpha_attractor_cycle){
#   tic()
#   for(p in p_0_cycle){
#     data_model <- conformist_transmission_discrete(N = 1000, p_0 = p, D = 1, alpha_attr = a, t_max = 50, r_max = 100)
#     output[output$p == p & output$a == a, ]$stabile <- sum(data_model[data_model$generation==50,]$p == 1)
#   }
#   print(a)
#   toc()
# }

# save file:
# write_csv(output, "data/fig3.csv")

# plot
output <- read_csv("data/fig3.csv") %>%
  mutate(stabile = stabile/100) 
  
ggplot(data = output, aes(x = a, y = p, fill = stabile)) +
  geom_tile(color = "black") +
  labs(y = expression(p[0]), 
       x = expression(alpha)) +
  theme_minimal() +
  scale_fill_gradient(low = "gray", high = "darkgreen", na.value = NA) +
  labs(fill = "Runs stable\nat trait A") +
  ggsave("figures/fig3.pdf", width = 5, height = 4)




