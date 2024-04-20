library(tidyverse)
library(ggthemes)

setwd("/Users/an2n/Desktop/BS\ BIO\ III_2nd\ sem/BIO\ 122\ -\ Animal\ Physio/Lab\ -\ 122/122\ -\ LabEx\ 7")

subject <- as.character(c(1:3))
duke <- c(39.75, 90, 60)
ivy <- c(62, 72, 112.5)
clot.s <- c(99, 42, 150)

exp <- data.frame(subject, duke, ivy, clot.s)

exp.long <- exp %>% 
  gather(key = "exp", value = "time", -c(subject)) 

exp.long %>% 
  filter(exp == "duke" | exp == "ivy") %>% 
  ggplot(aes(subject, time, fill = exp)) +
  geom_bar(stat = "identity", alpha = 0.95, color = "black") +
  geom_label(aes(label = time), fill = NA, vjust = -0.1, label.size = 0) +
  facet_grid(~exp, labeller = labeller(exp = c(duke = "Duke's Method", ivy = "IVY Method"))) +
  theme_base() +
  scale_y_continuous(expand = expansion(0), limits = c(0, 130)) +
  labs(x = "Subject", y = "Time (s)") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 15),
        plot.background = element_rect(color = NA),
        axis.ticks.x = element_blank())

# ggsave("rplot01.png", dpi = 300)

exp.long %>% 
  filter(exp == "clot.s") %>% 
  ggplot(aes(subject, time, fill = subject)) +
  geom_bar(stat = "identity", alpha = 0.95, color = "black") +
  geom_label(aes(label = time), fill = NA, vjust = -0.1, label.size = 0) +
  theme_base() +
  scale_y_continuous(expand = expansion(0), limits = c(0, 180)) +
  labs(x = "Subject", y = "Time (s)") +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA),
        axis.ticks.x = element_blank())

# ggsave("rplot02.png", dpi = 300)
