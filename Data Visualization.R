library(tidyverse)

ggplot(data=diamonds,aes(x=carat,y=price))+
  geom_point(alpha=0.1,position = "jitter")+
  facet_wrap(~cut,nrow=2)+
  geom_rug()+
  theme_bw()

ggplot(data=diamonds,aes(x=carat,y=price,color=cut))+
  geom_point(color="gray",alpha=0.1,position = "jitter")+
  geom_smooth()+
  facet_wrap(~cut,nrow=2)+
  geom_rug()+
  theme_bw()

ggplot(data = diamonds,
       mapping = aes(x = cut, y = depth, fill = cut)) +
  geom_jitter(width = 0.2, color = "gray", alpha = 0.75) +
  geom_violin(alpha=0.5) +
  theme_bw()

ggplot(data=diamonds,aes(x=depth))+
  geom_histogram()+
  geom_rug(mapping = aes(color=cut))+
  theme_bw()

ggplot(data=diamonds,aes(x=carat,y=price,color=cut))+
  geom_point(color="gray",alpha=0.1,position = "jitter")+
  geom_smooth()+
  facet_wrap(~cut,nrow=2)+
  geom_rug()+
  theme_bw()

ggplot(data=diamonds,aes(x=carat,y=price))+
  geom_point(alpha=0.1,position = "jitter")+
  facet_wrap(~cut,nrow=2)+
  geom_rug()+
  theme_bw()

ggplot(data=diamonds,aes(x=carat,y=price))+
  geom_point(alpha=0.1,position = "jitter")+
  facet_wrap(~cut,nrow=2)+
  theme_bw()
ggplot(data=diamonds,aes(x=carat,y=price))+
  geom_point(alpha=0.1,position = "jitter",color="gray")+
  facet_wrap(~cut,nrow=2)+
  geom_rug()+
  theme_bw()

ggplot(data=diamonds,aes(x=depth))+
  geom_histogram()+
  geom_rug(mapping = aes(color=cut))+
  theme_bw()

ggplot(data = diamonds,
       mapping = aes(x = cut, y = depth, fill = cut)) +
  geom_jitter(width = 0.2, color = "gray", alpha = 0.75) +
  geom_violin(alpha=0.5) +
  theme_bw()