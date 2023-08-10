library(tidyverse)
library(sqldf)
getwd()

GlobalAncestry <- read_csv("GlobalAncestry.csv")

train<-GlobalAncestry[GlobalAncestry$ancestry %in% c("African", "European", "EastAsian", "Oceanian","NativeAmerican"),]
train<- na.omit(train)

test<-GlobalAncestry[GlobalAncestry$ancestry %in% c("Unknown1", "Unknown2", "Unknown3", "Unknown4","Unknown5"),]
test<- na.omit(test)

testmex<-GlobalAncestry[GlobalAncestry$ancestry %in% c("Mexican"),]
testmex<-na.omit(testmex)


Y <- train %>% select(ancestry) %>% as.matrix()
X <- train %>% select(-ancestry) %>% as.matrix()

Y

lambdas <- 10^seq(-3,3, length.out=100)

install.packages("glmnet")
library(glmnet)

lasso.fit= glmnet(X,Y,alpha=1,lambda = lambdas, family = "multinomial")

help("glmnet")
plot(lasso.fit, xvar="lambda")

help("cv.glmnet")

GlobalAncestry <- read_csv("GlobalAncestry.csv")

train<-GlobalAncestry[GlobalAncestry$ancestry %in% c("African", "European", "EastAsian", "Oceanian","NativeAmerican"),]

test<-GlobalAncestry[GlobalAncestry$ancestry %in% c("Unknown1", "Unknown2", "Unknown3", "Unknown4","Unknown5"),]

testmex<-GlobalAncestry[GlobalAncestry$ancestry %in% c("Mexican"),]

lasso.cv<-cv.glmnet(X, Y, alpha = 1, lambda = lambdas, nfolds = 10,family = "multinomial")

plot(lasso.cv)

lasso.cv$lambda.min
simplest_lambda_value <- lasso.cv$lambda.1se

lasso.simplest <- glmnet(X, Y, alpha = 1, lambda = simplest_lambda_value, family = "multinomial")

new_Predict=predict(lasso.simplest,X, s=simplest_lambda_value,type = "class")

train.withPreds<-train %>% mutate(predicted_ancestry=new_Predict)

train.withPreds %>% select(ancestry,predicted_ancestry) %>% table()

train.withPreds %>% summarize(accuracy=mean(predicted_ancestry==ancestry))

X <- test %>% select(-ancestry) %>% as.matrix()

new_predict_test<-predict(lasso.simplest,X, s=simplest_lambda_value,type = "class")

test.withPreds<-test %>% mutate(predicted_ancestry_test=new_predict_test)

test.withPreds %>% select(ancestry,predicted_ancestry_test)

X<- testmex %>% select(-ancestry) %>% as.matrix()
new_predict_test_mex<-predict(lasso.simplest,X, s=simplest_lambda_value,type = "response")

test_mex_predicted_tibble<-new_predict_test_mex %>% as_tibble()

African<-test_mex_predicted_tibble %>% select(African.1)%>% mutate(Ancestry="African")

EastAsian<-test_mex_predicted_tibble %>% select(EastAsian.1)%>% mutate(Ancestry="EastAsian")

Oceanian<-test_mex_predicted_tibble %>% select(Oceanian.1)%>% mutate(Ancestry="Oceanian")

NativeAmerican<-test_mex_predicted_tibble %>% select(NativeAmerican.1)%>% mutate(Ancestry="Native_American")

European<-test_mex_predicted_tibble %>% select(European.1)%>% mutate(Ancestry="European")

ggplot() +
  geom_violin(data = African,mapping = aes(x=Ancestry,y=African.1),fill="orange")+
  geom_violin(data = European,mapping = aes(x=Ancestry,y=European.1),fill="blue")+
  geom_violin(data = EastAsian,mapping = aes(x=Ancestry,y=EastAsian.1),fill="pink")+
  geom_violin(data = Oceanian,mapping = aes(x=Ancestry,y=Oceanian.1),fill="green")+
  geom_violin(data = NativeAmerican,mapping = aes(x=Ancestry,y=NativeAmerican.1),fill="purple")+
  xlab("Ancestry")+
  ylab("Class Probabilities")+
  theme(legend.position = "none")
