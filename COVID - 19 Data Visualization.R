library("dplyr")
library("tidyr")

deaths <- read.csv('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')
confirmed <- read.csv('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')

#1
confirmed_stacked = confirmed %>%
  gather(key = Date, value = Confirmed, starts_with('X')) %>%
  select(UID,Province_State,Date,Confirmed) %>%
  filter(Province_State %in% state.name | Province_State == 'District of COlumbia')

deaths_stacked = deaths %>%
  gather(key = Date, value = Deaths, starts_with('X')) %>%
  select(UID,Province_State,Date,Population,Deaths) %>%
  filter(Province_State %in% state.name | Province_State == 'District of COlumbia')

#2
joined_df = confirmed_stacked %>%
  left_join(deaths_stacked,by=c("UID","Province_State","Date"))

#3
df3 = joined_df %>%
  mutate(Date = gsub("X","",Date),Date = chartr('.','/',Date),Date = as.Date(Date, "%m/%d/%y"))

#4
df4 = df3 %>%
  group_by(Province_State,Date)%>%
  summarise(Total.Deaths = sum(Deaths), Total.Population = sum(Population), Total.Confirmed = sum(Confirmed))

#5
clean_rates_df = df4 %>%
  mutate(Confirmed_rate = Total.Confirmed/Total.Population*100000,Death_rate = Total.Deaths/Total.Population*100000)

#6
clean_rates_df = joined_df %>%
  mutate(Date = gsub("X","",Date),Date = chartr('.','/',Date),Date = as.Date(Date, "%m/%d/%y"))%>%
  group_by(Province_State,Date)%>%
  summarise(Total.Deaths = sum(Deaths), Total.Population = sum(Population), Total.Confirmed = sum(Confirmed))%>%
  mutate(Confirmed.rate = ((Total.Confirmed/Total.Population)*100000),Death.rate = ((Total.Deaths/Total.Population)*100000))%>%
  data.frame()

#7
library(ggplot2)
ggplot(data = clean_rates_df) + 
  geom_line(aes(x = Date, y = Confirmed.rate)) + 
  facet_wrap(~Province_State) + 
  ylab("Confirmed (per 100K)")

