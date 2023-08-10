library(tidyverse)

ggplot(data=diamonds,aes(x=carat,y=price))+
  geom_point(alpha=0.1,position = "jitter")+
  facet_wrap(~cut,nrow=2)+
  geom_rug()+
  theme_bw()
