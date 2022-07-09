# load packages
library(tidyverse) # easy data wrangling
library(patchwork) # easy plot composition
library(ggridges) # ridgeline plots in ggplot2

# theme 
theme_set(theme_minimal())

# import data
balls <- read_csv("TG-Balls.csv") # imported as data frame 

# clean / wrangle data
selected_days <- c("Monday", "Tuesday", "Wednesday", "Thursday") # variable for plot filter on days with data collection

balls_fil <- balls %>% 
  filter(day %in% selected_days)

balls_fil$day <- factor(balls_fil$day, levels = c("Thursday", "Wednesday", "Tuesday", "Monday"))

# daily ball estimation ridgeline plot
p1 <- balls_fil %>%
  ggplot(aes(x = total_balls_estimation_mean, y = day, fill = factor(stat(quantile)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE,
                      rel_min_height = 0.00009,
                      quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability", 
                    values = c("#0000FFA0", "#A0A0A0A0", "#FF0000A0"),
                    labels = c("(0, 0.025)", "(0.025, 0.975)", "(0.975, 1)")) +
  labs(title = "Topgolf Greenville", subtitle = "Estimation - Daily Balls Dispensed", x = "", y = "", caption = "") +
  theme(legend.position = "bottom")

# balls per hour (locally weighted regression plot and lm)
p2 <- balls %>%
  ggplot(aes(x = balls_per_hour, y = practice_balls_per_hour)) +
  geom_smooth(method = loess, se = FALSE) +
  stat_smooth(method = lm, se = FALSE, aes(color = "red", alpha = .7)) +
  geom_jitter() +
  labs(title = "", subtitle = "Practice Balls per Hour vs Balls Dispensed per Hour", x = "Balls Per Hour", y = "Practice Balls Per Hour", caption = "") +
  theme(legend.position = "none")

# balls per hour box plot
balls_fil$day <- factor(balls_fil$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday"))
p3 <- balls_fil %>%
  ggplot(aes(x = day, y = balls_per_hour, fill = day, alpha = .7)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width", adjust = .5) +
  labs(title = "", subtitle = "Balls Dispensed per Hour", x = "", y = "", caption = "") +
  theme(legend.position = "none")

# practice balls per hour box plot
p4 <- balls_fil %>% 
  ggplot(mapping = aes(x = day, y = practice_balls_per_hour, fill = day, alpha = .7)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width", adjust = .7) +
  labs(title = "", subtitle = "Practice Balls Dispensed per Hour", x = "", y = "", caption = "") +
  theme(legend.position = "none")

# patchwork
(p1 + p3) / (p2 + p4)

# daily balls date col
p5 <- balls_fil %>%
  ggplot(aes(x = date, y = total_balls_estimation_mean, fill = day)) +
  geom_col(alpha = .7) +
  labs(x = "", y = "", title = "Estimated Balls Dispensed") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.title = element_blank()) 

# daily practice balls date col
p6 <- balls_fil %>%
  ggplot(aes(x = date, y = practice_balls, fill = day, label = practice_balls)) +
  geom_col(alpha = .7) +
  geom_text(vjust = 1.5) +
  labs(x = "", y = "", title = "Practice Balls Dispensed") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.title = element_blank())  

# patchwork
p5 / p6