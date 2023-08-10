library(tidyverse)
library(nycflights13)

flights %>%
  select(distance,dep_time) %>%
  mutate(canceled=is.na(dep_time)) %>%
  ggplot(mapping = aes(x=canceled,y=distance))+
  geom_boxplot(notch = TRUE)

flights %>%
  select(year,month,dep_time) %>%
  mutate(canceled=is.na(dep_time)) %>%
  group_by(year,month) %>%
  summarise(proportion=mean(canceled==TRUE)) %>%
  ggplot(mapping = aes(x=as.factor(month),y=proportion))+
  geom_col()

flights %>%
  select(year,month,day,distance) %>%
  group_by(year,month,day) %>%
  summarise(averageDistance=mean(distance),standardDeviationOfDistance=sd(distance)) %>%
  ggplot(mapping = aes(x=standardDeviationOfDistance,y=averageDistance))+
  geom_point()+
  geom_smooth(method = 'lm')