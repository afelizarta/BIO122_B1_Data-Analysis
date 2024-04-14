library(tidyverse)
library(ggthemes)
library(patchwork)
library(readxl)

setwd("/Users/an2n/Desktop/BS\ BIO\ III_2nd\ sem/BIO\ 122\ -\ Animal\ Physio/Lab\ -\ 122/122\ -\ LabEx\ 6")

# Peripheral perfusion -----
perfusion <- read_excel("Exercise 6_Physiology of Circulation.xlsx", 
                        sheet = "BLOCK B", range = "A5:I11")

perf.long <- perfusion %>% 
  gather(key = "key", value = "value", -c(`Group No.`, Age, Sex)) %>% 
  separate(key, c("part", "hem"), sep = "_")

perf.long %>% 
  group_by(hem, part) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  ggplot(aes(part, mean, fill = part, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", alpha = .95, color = "black") +
  geom_errorbar(width = 0.2) +
  facet_wrap(~hem) +
  labs(y = "Capillary Refill Time (s)", x = NULL) +
  theme_base() +
  scale_y_continuous(expand = expansion(0), limits = c(0, 5.5)) +
  scale_x_discrete(labels = c("Big Toe", "Forefinger",
                              "Forefinger after dipping\nin ice cold water for\n2 minutes")) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14.5),
        panel.grid.major = element_blank(),
        plot.background = element_rect(color = "white"))

# ggsave("rplot01.png", dpi = 300)

# Allen's test ------
allen <- read_excel("Exercise 6_Physiology of Circulation.xlsx", 
                    sheet = "BLOCK B", range = "A20:G26")

allen.long <- allen %>% 
  gather(key = "key", value = "value", -c(`Group No.`, Age, Sex)) %>% 
  separate(key, c("Arm", "artery"), sep = "_")

allen.plot <- allen.long %>% 
  group_by(Arm, artery) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  ggplot(aes(artery, mean, fill = artery, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", alpha = 0.95, color = "black") +
  geom_errorbar(width = 0.2) +
  facet_wrap(~Arm) +
  labs(y = "Arterial Perfusion Time (s)", x = NULL) +
  theme_base() +
  scale_y_continuous(expand = expansion(0), limits = c(0, 3.5)) +
  scale_x_discrete(labels = c("Occluded\nRadial Artery", "Occluded\nUlnar Artery")) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 14.5),
        plot.background = element_rect(color = "white"))

# ggsave("rplot02.png", dpi = 300)

allen.long %>% 
  ggplot(aes(artery, value, fill = artery)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 4) +
  facet_wrap(~Arm) +
  labs(y = "Time (s)", x = NULL) +
  theme_base() +
  scale_x_discrete(labels = c("Occluded\nRadial Artery", "Ocluded\nUlnar Artery")) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 14.5))

# Rubor -------
rubor <- read_excel("Exercise 6_Physiology of Circulation.xlsx", 
                    sheet = "BLOCK B", range = "A33:E39")

rubor.long <- rubor %>% 
  gather(key = "key", value = "value", -c(`Group No.`, Age, Sex)) 

rubor.plot <- rubor.long %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  ggplot(aes(key, mean, fill = key, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", alpha = 0.95, color = "black", width = 0.8) +
  geom_errorbar(width = 0.2) +
  labs(y = "Arterial Perfusion Time (s)", x = NULL) +
  theme_base() +
  scale_y_continuous(expand = expansion(0), limits = c(0, 18)) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 15),
        aspect.ratio = 9/9,
        plot.background = element_rect(color = "white"))

 # ggsave("rplot03.png", dpi = 300)
 
(allen.plot + rubor.plot) +
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(face = "bold", size = 25))
 
  ggsave("rplot02-03.png", dpi = 300)

# Peripheral Vein Collapse ------
pvc <- read_excel("Exercise 6_Physiology of Circulation.xlsx", sheet = "BLOCK B", range = "A88:E94")

pvc.plot <- pvc %>% 
  mutate(Hand = recode(Hand, Right= "Right Hand", Left = "Left Hand")) %>% 
  group_by(Hand) %>% 
  summarize(mean = mean(`Height (cm)`),
            sd = sd(`Height (cm)`)) %>% 
  ggplot(aes(Hand, mean, fill = Hand, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", alpha = 0.95, color = "black") +
  geom_errorbar(width = 0.2) +
  labs(y = "Height (cm)", x = "Peripheral Vein Collapse") +
  theme_base() +
  scale_y_continuous(expand = expansion(0), limits = c(0, 32)) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 15),
        plot.background = element_rect(color = "white"))

# ggsave("rplot04.png", dpi = 300)

# Neck ----
neck.veins <- read_excel("Exercise 6_Physiology of Circulation.xlsx", sheet = "BLOCK B", range = "A101:D107")

neckv.plot <- neck.veins %>% 
  ggplot(aes(x = "", y = `Height (cm) + 5cm`)) +
  geom_boxplot(fill = "skyblue3", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 1, size = 3) +
  theme_base() +
  labs(x = "Central Venous Pressure\nin neck veins", y = "Height (cm)")  +
  theme(axis.ticks.x = element_blank(),
        plot.background = element_rect(color = "white"))

 # ggsave("rplot05.png", dpi = 300)

(pvc.plot + neckv.plot) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 25))

# ggsave("rplot04-05.png", dpi = 300) 

