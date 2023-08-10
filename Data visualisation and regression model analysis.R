library(tidyverse)
library(ISLR)

head(College)

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

College.recoded

lm.fit<-lm(Private~.,College.recoded)

summary(lm.fit)

lm.fit<-lm(Private~Apps,College.recoded)

summary(lm.fit)

College.recoded %>%
  ggplot(mapping=aes(x=Apps,y=Private)) +
  geom_point() +
  geom_smooth(method="lm")

extractedAppsDF<-data.frame(Apps=College.recoded$Apps)

predict(lm.fit,extractedAppsDF)
predict(lm.fit, tibble(Apps=500))
predict(lm.fit)

extractedAppsDF<-data.frame(Apps=College.recoded$Apps)

lm.fit<-lm(Private~Apps,College.recoded)
College.recoded %>%
  mutate(Predicted_Private=ifelse(predict(lm.fit)>0.5,1,0))

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

lm.fit<-lm(Private~.,College.recoded)

College.recoded %>%
  mutate(Predicted_Private_Multiple=ifelse(predict(lm.fit)>0.5,1,0))

lm.fit<-lm(Private~.,College.recoded)

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

College.recoded.New<-College.recoded[,2:18]

lm.fit<-lm(Private~.,College.recoded)

College.recoded %>%
  mutate(Predicted_Private_Multiple=ifelse(predict(lm.fit,College.recoded.New)>0.5,1,0))

glm.fit<-glm(Private~.,College.recoded,family="binomial")

summary(glm.fit)

glm.fit<-glm(Private~Outstate,College.recoded,family="binomial")

summary(glm.fit)

College.recoded %>%
  ggplot(mapping = aes(x=Outstate,y=Private)) +
  geom_point() +
  geom_smooth(method = "glm",method.args=c(family="binomial"))

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

glm.fit<-glm(Private~Outstate,College.recoded,family="binomial")

College.recoded %>%
  mutate(Predicted_Private_SimpleLogistic=ifelse(predict(glm.fit,type="response")>0.5,1,0))

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

lm.fit<-lm(Private~Apps,College.recoded)

College.recoded.PredictedSimpleLinear<-College.recoded %>%
  mutate(Predicted_Private_SimpleLinear=ifelse(predict(lm.fit)>0.5,1,0))

College.recoded.PredictedSimpleLinear %>%
  select(Private,Predicted_Private_SimpleLinear) %>%
  table()

College.recoded.PredictedSimpleLinear %>%
  summarize(accuracy=mean(Predicted_Private_SimpleLinear==Private))

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

lm.fit<-lm(Private~.,College.recoded)

College.recoded.PredictedMultipleLinear<-College.recoded %>%
  mutate(Predicted_Private_MultipleLinear=ifelse(predict(lm.fit)>0.5,1,0))

College.recoded.PredictedMultipleLinear %>%
  select(Private,Predicted_Private_MultipleLinear) %>%
  table()

College.recoded.PredictedMultipleLinear %>%
  summarize(accuracy=mean(Predicted_Private_MultipleLinear==Private))

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

glm.fit<-glm(Private~Outstate,College.recoded,family="binomial")

College.recoded.PredictedSimpleLogistic<-College.recoded %>%
  mutate(Predicted_Private_SimpleLogistic=ifelse(predict(glm.fit,type="response")>0.5,1,0))

College.recoded.PredictedSimpleLogistic %>%
  select(Private,Predicted_Private_SimpleLogistic) %>%
  table()

College.recoded.PredictedSimpleLogistic %>%
  summarize(accuracy=mean(Predicted_Private_SimpleLogistic==Private))

College.recoded<-College %>%
  mutate(Private=ifelse(Private=="Yes",1,0))

glm.fit<-glm(Private~.,College.recoded,family="binomial")

College.recoded.PredictedMultipleLogistic<-College.recoded %>%
  mutate(Predicted_Private_MultipleLogistic=ifelse(predict(glm.fit,type="response")>0.5,1,0))

College.recoded.PredictedMultipleLogistic %>%
  select(Private,Predicted_Private_MultipleLogistic) %>%
  table()

College.recoded.PredictedMultipleLogistic %>%
  summarize(accuracy=mean(Predicted_Private_MultipleLogistic==Private))

College.recoded %>%
  ggplot(mapping = aes(x=Outstate,y=Private)) +
  geom_point() +
  geom_smooth(method = "glm",method.args=c(family="binomial"))