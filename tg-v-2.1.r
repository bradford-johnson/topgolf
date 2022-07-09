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

# colors for plots
tg_colors <-c("#F36E2B", "#46A3D8", "#F2B33D", "#A0D94A")

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
  theme(legend.position = "bottom", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# fix day factor
balls_fil$day <- factor(balls_fil$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday"))

# balls per hour (locally weighted regression plot and lm)
p2 <- balls %>%
  ggplot(aes(x = balls_per_hour, y = practice_balls_per_hour)) +
  geom_smooth(method = loess, se = FALSE) +
  stat_smooth(method = lm, se = FALSE, aes(color = "red", alpha = .7)) +
  geom_jitter() +
  labs(title = "", subtitle = "Practice Balls per Hour vs Balls Dispensed per Hour", x = "Balls Per Hour", y = "Practice Balls Per Hour", caption = "") +
  theme(legend.position = "none", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# practice balls percent day view
p3 <- balls_fil %>%
  ggplot(aes(x = day, y = decimal, fill = day)) +
  geom_rug(outside = TRUE, sides = "r", alpha = 1/2, position = "jitter") +
  coord_cartesian(clip = "off") +
  geom_boxplot(alpha = 0.7, fill = tg_colors) +
  labs(x = "", y = "Practice Ball Percentage", title = "", subtitle = "Daily Practice Ball Distribution") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.position = "none", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

# practice balls percent over time
p4 <- balls_fil %>%
  ggplot(aes(x = date, y = decimal, group = 1)) +
  geom_point(aes(color = day)) +
  scale_color_manual(values = tg_colors) +
  geom_line(alpha = 0.7) + 
  geom_hline(yintercept = .2, linetype = 3) +
  geom_hline(yintercept = .12, linetype = 3) +
  labs(x = "", y = "Practice Ball Percentage", title = "", subtitle = "Practice Ball Trends by Day") +
  facet_wrap(~day) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none", axis.text.x = element_blank())

# patchwork
(p1 + p2) / (p3 + p4)