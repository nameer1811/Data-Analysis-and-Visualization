library(dplyr)
library(tidyr)
library(arules)
library(choroplethr)
library(choroplethrMaps)

train_dockets = read.csv("C:/Users/jt7372wd/Desktop/Spring 2020/DSci 210/Assignments/Project 2/train_dockets.csv")
district_fips_code = read.csv("C:/Users/jt7372wd/Desktop/Spring 2020/DSci 210/Assignments/Project 2/district_fips_code.csv")
districts = read.csv("C:/Users/jt7372wd/Desktop/Spring 2020/DSci 210/Assignments/Project 2/districts.csv")
train_other_motions = read.csv("C:/Users/jt7372wd/Desktop/Spring 2020/DSci 210/Assignments/Project 2/train_other_motions.csv")
train_terminating_motions = read.csv("C:/Users/jt7372wd/Desktop/Spring 2020/DSci 210/Assignments/Project 2/train_terminating_motions.csv")

#Question 1
differed_circuits = train_dockets %>%
  group_by(circuit) %>%
  summarise(summary_judgment = mean(summary_judgment, na.rm = TRUE), settled = mean(settled, na.rm = TRUE))%>%
  data.frame()

#Question 2
nfiles = train_dockets %>%
  group_by(district) %>%
  summarize(nfiles = n()) %>%
  mutate(district_number = district) %>%
  select(-district)

county_info = district_fips_code %>%
  left_join(districts, by = c("state", "district_number")) %>%
  left_join(nfiles, by = c("district_number")) %>%
  mutate(filing_rate = (nfiles/census_2010_population)*100000) %>%
  mutate(region = fips_code, value = filing_rate)

county_choropleth(county_info, state_zoom =tolower(unique(county_info$state)), 
                  legend='Filing rate\n per 100,000')

#Question 3
#A
other_motions = train_other_motions %>%
  select(motion_type,mudac_id) %>%
  group_by(motion_type,mudac_id) %>%
  summarise(count = n()) %>%
  spread(key = motion_type, value = count ) %>%
  gather(key= "motions", value = "data", "Motion for Discovery Sanctions":"Motion to Compel Discovery")%>%
  mutate(data = as.numeric(ifelse(is.na(data), "0", "1" )))%>%
  spread(key = motions, value = data)

terminating_motions = train_terminating_motions %>%
  select(motion_type, mudac_id) %>%
  group_by(motion_type, mudac_id) %>%
  summarize(count = n()) %>%
  spread(key = motion_type, value = count)%>%
  gather(key= "motions", value = "data", "Motion for Default Judgment" : "Motion to Transfer Venue") %>%
  mutate(data = as.numeric(ifelse(is.na(data), "0", "1" )))%>%
  spread(key = motions, value = data)

final_df = train_dockets %>%
  select(mudac_id, settled) %>%
  inner_join(other_motions, by= c("mudac_id")) %>%
  inner_join(terminating_motions, by= c("mudac_id")) %>%
  arrange(mudac_id)

#B
task3B= final_df %>%
  select(-mudac_id)%>%
  gather(key = "LHS", value = "Used_LHS", -settled) %>%
  mutate(Used_Both = settled * Used_LHS) %>%
  group_by(LHS) %>%
  summarise( sup_SET = sum(settled)/13450, sup_LHS = sum(Used_LHS)/13450, sup_Both = sum(Used_Both)/13450)%>%
  mutate(confidence = sup_Both/sup_LHS) %>%
  mutate(lift = confidence / sup_SET) %>%
  filter(sup_Both > 0.10) %>%
  arrange(-lift) %>%
  data.frame()
#C
task3C = final_df %>%
  mutate_if(is.integer, as.numeric)%>%
  mutate_if(is.numeric, as.factor) %>%
  apriori(parameter = list(support = 0.15, confidence = 0.2, maxlen = 3)) %>%
  subset(rhs %in% "settled=1")%>%
  head(10, by= "lift")%>%
  inspect()
# The lift is 1.141, which means that the liklihood of settled being 1 goes up by
# 14.1% if the Motion for Discovery Sanctions is 0 and Motion for Protective Order
# is 1.
