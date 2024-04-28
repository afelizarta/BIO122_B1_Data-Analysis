library(tidyverse)
library(scales)
library(ggthemes)
library(patchwork)
library(readxl)

setwd("/Users/an2n/Desktop/BS\ BIO\ III_2nd\ sem/BIO\ 122\ -\ Animal\ Physio/Lab\ -\ 122/122\ -\ LabEx\ 8")

# Changes in thoracic cavity -----
thor.cav <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "A6:G12")

thor.cav %>% 
  gather(key = "key", value = "value", -c(`Group No.`)) %>%
  mutate(key = factor(key, levels = c("Tidal inspiration", "Tidal expiration", "Forceful inspiration after a tidal expiration",  
                      "Forceful expiration after a tidal inspiration", "Forceful inspiration after a forceful expiration",
                      "Forceful expiration after a forceful inspiration"))) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(label = sprintf("%.2f ± %.2f", mean, sd)) %>% 
  ggplot(aes(key, mean, fill = key, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(width = 0.2) +
  geom_label(aes(label = label), fill = NA, vjust = -5.8, label.size = 0) +
  coord_cartesian(ylim = c(77, 102)) +
  scale_x_discrete(labels = label_wrap(15)) +
  theme_base() +
  labs(x = "Changes in the thoracic cavity", y = "Chest circumference (cm)") +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA),
        axis.ticks.x = element_blank())

# Normal Respiratory Rate----
subject <- as.character(c(1:6))
rr <- c(14, 19, 15, 17, 15, 15)
norm.rr <- data.frame(subject, rr)

mean(norm.rr$rr)
sd(norm.rr$rr)

p1 <- norm.rr %>% 
  ggplot(aes(subject, rr)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue4") +
  coord_cartesian(ylim = c(10, 20)) +
  labs(x = "Subject", y = "Resting respiratory\nrate (breaths/min)") +
  geom_hline(yintercept = 15.83333, color = "red3", size = 1) +
  theme_base() +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        plot.background = element_rect(color = NA)) +
  annotate(geom = "text", y = 20, x = 1, label = "15.83 ± 1.83",
           size = 4, color = "red3")

# load-----
bh1 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "A29:B35")
bh2 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B40:B46")
bh3 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B51:B57")
bh4 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B62:B68")
bh5 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B73:B79")

rr1 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B84:B90")
rr2 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B95:B101")
rr3 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B106:B112")
rr4 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B117:B123")
rr5 <- read_excel("Exercise 8_Respiratory Function.xlsx", sheet = "BLOCK B", range = "B128:B134")

# Breath holding-----
resp.exc <- bind_cols(bh1, bh2, bh3, bh4, bh5, rr1, rr2, rr3, rr4, rr5)

resp.exc.summ <- resp.exc %>% 
  gather(key = "key", value = "value", -c(`Group No.`)) %>% 
  separate(key, c("part", "act"), sep = "_") %>% 
  mutate(act = factor(act, levels = c("After a Quiet Inspiration",
                                      "After a Quiet Expiration",
                                      "After a Deep Inspiration",
                                      "Hyperventilation",
                                      "Breathing through a Closed System",
                                      "Concentration",
                                      "Partial Nostril Occlussion",
                                      "Mouth Breathing",
                                      "Reading Passages",
                                      "Running in Place"))) %>% 
  group_by(part, act) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(label = sprintf("%.2f ± %.2f", mean, sd))

p2 <- resp.exc.summ %>% 
  filter(part == "Breath Holding") %>% 
  ggplot(aes(act, mean, fill = act, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(width = 0.2) + 
  geom_text(aes(label = label, y = mean + sd + 5), vjust = 0.5) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 110)) +
  theme_base() +
  labs(x = NULL, y = "Breath holding\ntime (sec)") +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA),
        axis.ticks.x = element_blank())

p3 <- resp.exc.summ %>% 
  filter(part == "Respiratory Rate") %>% 
  ggplot(aes(act, mean, fill = act, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(width = 0.2) + 
  geom_text(aes(label = label, y = mean + sd + 5), vjust = 3) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(expand = expansion(0), limits = c(0, NA)) +
  theme_base() +
  labs(x = NULL, y = "Respiratory rate\n(breaths/min)") +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA),
        axis.ticks.x = element_blank())

(p1 / p2 / p3) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 20))
  

