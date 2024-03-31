library(tidyverse)
library(ggthemes)
library(readxl)

# Auscultation -----
ausc_dat <- read_excel("Exercise 5_Cardiac Function.xlsx", sheet = "BLOCK B", range = "A5:O11")

ausc.long<- ausc_dat %>% 
  gather(key = "key", value = "value", -c(Group, Age, Sex, `Body Weight`,
                                          `Level of Fitness`, `Hemoglobin Level`, Lifestyle)) %>% 
  separate(key, c("Auscultation point", "Stethoscope"), sep = "_")

ausc.summ <- ausc.long %>% 
  select(Group, Sex, `Level of Fitness`, `Auscultation point`, Stethoscope, value) %>% 
  group_by(`Auscultation point`, Stethoscope) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(`Auscultation point` = factor(`Auscultation point`, levels = c("Mitral",
                                                                        "Tricuspid",
                                                                        "Pulmonary",
                                                                        "Aortic")),
         Stethoscope = factor(Stethoscope, levels = c("Diaphragm", "Bell")))

ausc.summ %>% 
  ggplot(aes(Stethoscope, `Auscultation point`, fill = mean)) +
  geom_tile(color = "black")  +
  geom_text(aes(label = round(mean, 2)), color = "white", size = 4) +
  scale_fill_continuous(trans = 'reverse', low = "#D81B60", high = "#FF9999") +
  labs(x = "Stethoscope chestpiece",
       caption = "No heart murmurs were detected.",
       tag = "1 - Loudest 
       
       
       
       
       
4 - Faintest") +
  guides(fill = guide_colorbar(title = "Auditory intensity",
                               label = FALSE)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = rel(1.08)),
        axis.text = element_text(size = rel(1.08)),
        axis.text.x = element_text(margin = margin(t = -6, b = 3), color = "black"),
        axis.text.y = element_text(margin = margin(r = -8, l = 3), color = "black"), 
        legend.title = element_text(size = 12),
        plot.tag = element_text(size = 11),
        plot.tag.position = c(0.89, 0.54),
        plot.caption = element_text(size = 10))

 ggsave("rplot01.png", dpi = 300)

# Heart Rate and Positions -----
hr.pos_dat <- read_excel("Exercise 5_Cardiac Function.xlsx", sheet = "BLOCK B", range = "A18:T24")

hr.pos.long <- hr.pos_dat %>% 
  gather(key = "key", value = "value", -c(Group, Age, Sex, `Body Weight`,
                                          `Level of Fitness`, `Hemoglobin Level`, Lifestyle)) %>% 
  separate(key, c("Position", "Day"), sep = "_") %>% 
  mutate(`Level of Fitness` = factor(`Level of Fitness`, levels = c("Good", "Adequate", "Very poor")))

# Resting heart rate
hr.pos.long %>% 
  mutate(Group = as.character(Group)) %>% 
  filter(Day == 0) %>% 
  ggplot(aes(Group, value, fill = `Level of Fitness`)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.9) +
  geom_text(aes(label = Sex), vjust = -0.5) +
  coord_cartesian(ylim = c(40, 105)) +
  labs(x = "Subject",
       y = "Resting heart rate (bpm)") +
  theme_classic() +
  theme(axis.text = element_text(size = rel(1), color = "black"), 
        axis.ticks.x = element_blank())

# ggsave("rplot02.png", dpi = 300)

hr.pos.summ <- hr.pos.long %>% 
  mutate(`Level of Fitness` = recode(`Level of Fitness`, "Very poor" = "Non-athletic"),
         `Level of Fitness` = recode(`Level of Fitness`, "Adequate" = "Non-athletic"),
         `Level of Fitness` = recode(`Level of Fitness`, "Good" = "Athletic")) %>% 
  filter(Day > 0) %>% 
  select(-c(Age, `Body Weight`, `Hemoglobin Level`, Lifestyle)) %>% 
  group_by(Day, `Level of Fitness`, Position) %>% 
  summarize(mean = mean(value),
            sd = sd(value))

bf.line <- hr.pos.long %>% 
  mutate(`Level of Fitness` = recode(`Level of Fitness`, "Very poor" = "Non-athletic"),
         `Level of Fitness` = recode(`Level of Fitness`, "Adequate" = "Non-athletic"),
         `Level of Fitness` = recode(`Level of Fitness`, "Good" = "Athletic")) %>% 
  filter(Day > 0) %>% 
  select(-c(Age, `Body Weight`, `Hemoglobin Level`, Lifestyle)) %>% 
  group_by(`Level of Fitness`, Position) %>% 
  summarize(mean = mean(value),
            sd = sd(value))

# Resting heart rate and different positions
hr.pos.summ %>% 
  ggplot(aes(Day, mean, fill = Position, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(width = 0.2) +
  geom_hline(data = bf.line, aes(yintercept = mean), color = "red", size = 1) +
  coord_cartesian(ylim = c(40, 120)) +
  facet_grid(Position ~ `Level of Fitness`) +
  labs(y = "Heart rate (bpm)") +
  theme_base() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 14))

# ggsave("rplot03.png", dpi = 300)

# Heart Rate and Exercise -----
hr.exc_dat <- read_excel("Exercise 5_Cardiac Function.xlsx", sheet = "BLOCK B", range = "A32:R38")

hr.exc.long <- hr.exc_dat %>%   
  gather(key = "key", value = "value", -c(Group, Age, Sex, `Body Weight`,
                                          `Level of Fitness`, `Hemoglobin Level`, Lifestyle)) %>% 
  separate(key, c("description", "time"), sep = "_") %>%
  mutate(time = as.numeric(time))


hr.exc.long %>% 
  mutate(Group = as.character(Group)) %>% 
  ggplot(aes(time, value, group = Group, color = Group)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Sex, labeller = labeller(Sex = c("F" = "Female", "M" = "Male"))) +
  geom_vline(xintercept = 2.18, color = "orange3", linetype = "dashed") +
  geom_vline(xintercept = 12.18, color = "skyblue4", linetype = "dashed") +
  geom_hline(yintercept = 138.7166667, color = "red3", linetype = "dashed", linewidth = 0.8) +
  labs(x = "Time (min)",
       y = "Heart rate (bpm)",
       color = "Subject") +
  theme_base() +
  theme(strip.text = element_text(face = "bold", size = 15.5)) +
  annotate(geom = "text", x = -2.3, y = 185, label = "Warm up", color = "orange3", hjust = 0, size = 3.5) +
  annotate(geom = "text", x = 2.5, y = 185, label = "Low impact cardio workout", color = "red3", hjust = 0, size = 3.5) +
  annotate(geom = "text", x = 15, y = 185, label = "Cooldown", color = "skyblue4", hjust = 0, size = 3.5) +
  annotate(geom = "text", x = -2.7, y = 144, label = "Target zone", hjust = 0, color = "red3", size = 3.5)

# ggsave("rplot04.png", dpi = 300)
