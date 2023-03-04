# call necessary functions and libraries:

source("functions_prestige.R")


# FIGURE 4:

# FIGURE 4 A
tic()
data_model <- biased_transmission_demonstrator_discrete(N = 1000, p_0 = 0.5, c_s = 0.01, c_low = 0.001, alpha_attr = 0, t_max = 100, r_max = 100)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "p (proportion of individuals with trait A)") +
  ggsave("figures/fig4A.pdf", width = 4, height = 4)
toc()


# FIGURE 4 B
tic()
data_model <- biased_transmission_demonstrator_discrete(N = 1000, p_0 = 0.5, c_s = 0.01, c_low = 0.001, alpha_attr = 0.1, t_max = 100, r_max = 100)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "p (proportion of individuals with trait A)") +
  ggsave("figures/fig4B.pdf", width = 4, height = 4)
toc()


#