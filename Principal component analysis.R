library(glmnet)
library(tidyverse)

features_X<-GlobalAncestry[,-1]

pca<-prcomp(features_X)
Z<-pca$x %>% as_tibble()

Z.withAncestry<-Z %>% mutate(GlobalAncestry[,c("ancestry")])

Z.withAncestry.Modified<-Z.withAncestry %>% select(PC1,PC2,ancestry)

Z.withAncestry.Modified.filteredByOthers <- Z.withAncestry.Modified %>%
  filter(ancestry=="African" | ancestry=="European" | ancestry=="EastAsian" | ancestry=="Oceanian" | ancestry=="NativeAmerican" | ancestry=="Mexican")

Z.withAncestry.Modified.filteredByUnknown <- Z.withAncestry.Modified %>% 
  filter(ancestry=="Unknown1" | ancestry=="Unknown2" | ancestry=="Unknown3" | ancestry=="Unknown4" | ancestry=="Unknown5")

ggplot()+
  geom_point(data=Z.withAncestry.Modified.filteredByOthers,mapping = aes(x=PC1,y=PC2,color=ancestry),shape=16)+
  geom_point(data=Z.withAncestry.Modified.filteredByUnknown,mapping = aes(x=PC1,y=PC2,color=ancestry),shape=15,size=5)

var_explained=pca$sdev^2/sum(pca$sdev^2)

qplot(c(1:242), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)