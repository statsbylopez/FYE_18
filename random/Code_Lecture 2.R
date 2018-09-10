

library(Lahman)
library(tidyverse)
Batting1 <- Batting %>% 
  filter(yearID == 2016, AB > 100) 
Batting1 %>% head()


Master %>% 
  select(playerID:birthYear, nameGiven, weight, height, bats, throws, birthDate) %>% 
  head()

ggplot(data = Batting1, aes(x = X2B, y = HR)) + 
  geom_point() + 
  geom_smooth(method = "lm")

Batting1 %>% 
  summarise(cor.hr.x2b = cor(X2B, HR))

Batting1 %>% 
  summarise(cor.hr.X2B = cor(X2B, HR), 
            cor.hr.X3B = cor(X3B, HR), 
            cor.hr.X2B = cor(CS, HR), 
            cor.hr.X3B = cor(BB, HR), 
            cor.hr.X3B = cor(BB, HR))


Batting2 <- Batting %>% 
  filter(yearID == 2015|yearID == 2016, AB > 100) %>% 
  arrange(playerID) %>% 
  group_by(playerID) %>% 
  mutate(SB.next = lead(SB, 1), 
         HR.next = lead(HR, 1), 
         RBI.next = lead(RBI, 1), 
         BB.next = lead(BB, 1))

Batting2 %>% 
  ungroup() %>% 
  summarise(SB.cor = cor(SB, SB.next, use = "pairwise"), 
            HR.cor = cor(HR, HR.next, use = "pairwise"), 
            RBI.cor = cor(RBI, RBI.next, use = "pairwise"))
