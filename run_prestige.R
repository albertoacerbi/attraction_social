# call necessary functions and libraries:

source("functions_prestige.R")


# FIGURE 4:

# FIGURE 4 A
tic()
data_model <- biased_transmission_demonstrator_discrete(N = 1000, p_0 = 0.5, c_s = 0.01, c_copy = 5, alpha_attr = 0, t_max = 200, r_max = 100)
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
data_model <- biased_transmission_demonstrator_discrete(N = 1000, p_0 = 0.5, c_s = 0.01, c_copy = 5, alpha_attr = 0.1, t_max = 200, r_max = 100)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "p (proportion of individuals with trait A)") +
  ggsave("figures/fig4B.pdf", width = 4, height = 4)
toc()


# FIGURE 4 C
tic()
data_model <- biased_transmission_demonstrator_continuous(N = 1000, p_0 = NA, c_s = 0.01, c_copy = 5, alpha_attr = 0, t_max = 200, r_max = 100,
                                                          beta = 0.5, sd_error = .1)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1)) +
  ylim(c(-.5, 1.5)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = expression(bar(p)))

ggsave("figures/fig4C.pdf", width = 4, height = 4)
toc()


# FIGURE 4 D
tic()
data_model <- biased_transmission_demonstrator_continuous(N = 1000, p_0 = NA, c_s = 0.01, c_copy = 5, alpha_attr = 0.1, t_max = 200, r_max = 100,
                                                          beta = 0.5, sd_error = .1)
ggplot(data = data_model, aes(y = p, x = generation)) +
  geom_line(aes(group = run), colour = "coral") +
  stat_summary(fun = mean, geom = "line", size = 1) +
  ylim(c(0, 1.2)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = expression(bar(p)))

  ggsave("figures/fig4D.pdf", width = 4, height = 4)
toc()

# FIGURE 5

# alpha = 0.01

# c_s_cycle = c(0.005, 0.01, 0.1)
# c_copy_cycle = c(1, 5, 10)
# output <- tibble(c_s = rep(c_s_cycle, each = length(c_copy_cycle)),
#                 c_copy = rep(c_copy_cycle, length(c_s_cycle)),
#                 stabile = as.numeric(NA))
# 
# for(c_s in c_s_cycle){
#   tic()
#   for(c_copy in c_copy_cycle){
#     data_model <- biased_transmission_demonstrator_discrete(N = 1000, p_0 = 0.5, c_s = c_s, c_copy = c_copy, alpha_attr = 0.01, t_max = 200, r_max = 100)
#     output[output$c_s == c_s & output$c_copy == c_copy, ]$stabile <- sum(data_model[data_model$generation==200,]$p == 1)
#   }
#   print(c_s)
#   toc()
# }
# write_csv(output, "data/fig5.csv")

output <- read_csv("data/fig5.csv") %>%
  mutate(stabile = stabile/100) %>%
  mutate(c_s = as_factor(c_s)) %>%
  mutate(c_copy = as_factor(c_copy))

ggplot(data = output, aes(y = stabile, x = c_copy, group = c_s, colour = c_s)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.5)) +
  labs(x = expression(C[copy]), y = "Runs stable at trait A") +
  labs(colour = expression(C[s])) +
  scale_color_brewer(palette = "Paired") +
  ggsave("figures/fig5.pdf", width = 4, height = 4)





