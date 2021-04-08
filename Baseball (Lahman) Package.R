library("dplyr")
library("tidyr")
library(Lahman)
B = Batting %>%
  select("playerID", "H","AB")%>%
  group_by(playerID)%>%
  summarise(Career_H = sum(H,na.rm = TRUE), Career_AB = sum(AB,na.rm = TRUE))
#I used na.rm to not count the missing values in the data. This resulted in not avoiding players who had missing games.
F = Fielding %>%
  select("playerID", "POS", "GS")%>%
  filter(POS == "C")%>%
  group_by(playerID)%>%
  summarize(GS = sum(GS,na.rm = TRUE))%>%
  filter(GS >=500)
P = People %>%
  select("playerID", "nameFirst","nameLast")%>%
  mutate(Fullname = paste(nameFirst,nameLast," "))

BFP = F %>%
  left_join(B, by = c("playerID"))%>%
  left_join(P, by = c("playerID"))%>%
  data.frame()

Catching_full = BFP%>%
  mutate(POS = "C")%>%
  mutate(batting_avg = Career_H/Career_AB)%>%
  arrange(-batting_avg)%>%
  mutate(BA_rank = (1:198))%>%
  data.frame()


top10 = Catching_full %>% 
  filter(BA_rank <=10) %>% 
  mutate(Fullname = reorder(Fullname, BA_rank))
ggplot(data = top10) + 
  geom_point(aes(x = Fullname, y = batting_avg),size = 2)  + 
  geom_segment(aes(x = Fullname, xend = Fullname, y = 0.285, yend = batting_avg)) + 
  scale_y_continuous(breaks = seq(.285,.31, by=.005)) + 
  ylab('Career batting average') + xlab('Catcher') + 
  theme(axis.text.x = element_text(angle = 90))