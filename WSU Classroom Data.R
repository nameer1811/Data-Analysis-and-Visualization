library(tidyr)
library(dplyr)
library(ggplot2)

#Read in Classroom data
ClassroomData = read.csv("C:/Users/jt7372wd/Desktop/Fall 2020/DSCI 310/Task 6/WSU Classroom Use Gildemeister Dataset.csv")

#Pre-processing of data, e.g. remove FINAL EXAM, create StartTime, EndTime variables, etc. Adding colum for full join
ClassroomData= ClassroomData %>%
  mutate(byvariable=1) %>% 
  mutate(InstructorNum = if_else(Instructor == 'FINAL EXAM',"1","0")) %>%
  filter(InstructorNum == "0") %>%
  select(-Section,-Instructor,-InstructorNum,-Dates,-Course) %>%
  separate(col = Time, into = c("StartTime","EndTime"), sep = "\\-") %>%
  transform(StartTime = as.integer(StartTime), EndTime = as.integer(EndTime)) %>%
  mutate( EndTimeFilter = if_else(EndTime >= 1600, "1", "0")) %>%
  filter(EndTimeFilter == "0") %>%
  select(WeekDay, StartTime, EndTime, byvariable)

#Creating the Time slots
TimeData<-data.frame(byvariable=c(1),
                     Times=c(800,830,900,930,1000,1030,1100,1130,1200,1230,1300,1330,1400,1430,1500,1530))

#Completing the full join and adding time busy to count the time that is busy
ClassroomDataClean = full_join(ClassroomData,TimeData) %>%
  mutate(TimeBusy = ifelse((Times >= StartTime & Times <= EndTime),1,0)) %>%
  mutate(WeekDays = factor(WeekDay,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday"))) %>%
  mutate(Times = as.factor(Times)) %>%
  mutate(Times = factor(Times, levels = rev(levels(Times)))) %>%
  group_by(WeekDays, Times) %>%
  summarise(Count = sum(TimeBusy))

#Plotting the graph
ggplot(ClassroomDataClean, aes(WeekDays, Times, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high="red")

