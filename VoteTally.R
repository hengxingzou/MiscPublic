# Vote tally for student invited seminar speaker, Rice University EEB
# Originally created by Joshua Fowler
# Updated: Aug 1, 2022

library(tidyverse)
speaker <- read.csv("~/Downloads/Results.csv") %>%
  select(-Timestamp)

speaker2 <- speaker %>%
  pivot_longer(cols = First.Choice:Third.Choice, names_to = c("rank")) %>%
  rename(speaker = value) %>%
  drop_na()

speaker3 <- speaker2 %>%
  mutate(points = case_when(rank == "First.Choice" ~ 3,
                            rank == "Second.Choice" ~ 2,
                            rank == "Third.Choice" ~ 1)) %>%
  group_by(speaker)%>%
  summarise(total = sum(points))


plot <- ggplot(speaker3, aes(x = reorder(speaker, -total), y=total, fill=speaker))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=30, l=30)))+
  theme(legend.position = "none")+
  ggtitle("Student invited speaker nominee point totals") + 
  labs(x = "Speaker", y = "Total Points")

plot
