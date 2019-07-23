setwd("C:/Users/pc/Google Drive/Rutgers/Data Mining for Business Intelligence/FInal P")
library(readxl)
library(fpp)
library(data.table)
data <- read_excel("weatherAUS.xlsx")
setDT(data)
View(data)
train <- data[Date<="2014-12-31",]
test <-data[Date>"2014-12-31",]
dim(train)
dim(test)

#Logistic Regression
glm.fit = glm(RainTomorrow~MaxTemp + Rainfall + WindSpeed9am + Humidity9am+ Pressure9am+ Temp9am+ RainToday + Date, data=train,family = "binomial")
step.model <- glm.fit %>% stepAIC(trace = FALSE) #Stepwise regression to choose the best model
coef(step.model)
summary(step.model)

glm.pred = predict.glm(step.model,newdata = test,type = "response")
glm.pred=ifelse(glm.pred>0.5,1,0)
RainTomorrow = test$RainTomorrow
table(glm.pred, RainTomorrow)
mean(glm.pred == RainTomorrow)


#Linear Discriminant Analysis

lda.fit <- lda((RainTomorrow~MaxTemp+Rainfall+WindSpeed9am+Humidity9am+Pressure9am+Temp9am+RainToday),data = train)
lda.fit

plot(lda.fit)

lda.pred = predict(lda.fit,test)
names(lda.pred)
table(lda.pred$class, test$RainTomorrow)
mean(lda.pred$class==test$RainTomorrow)

#KNN

train.fall <- train[,RainTomorrow]
test.fall <- test[,RainTomorrow]

train.x <- train[,3:10]
test.x <- test[,3:10]

set.seed(1)
library(class)
knn.pred1 = knn(train.x, test.x, train.fall, k=1)
table(knn.pred1, test.fall)
(25181+3545)/(25181+3545+4826+4664) 
#75.17% When K=1

knn.pred3 = knn(train.x, test.x, train.fall, k=3)
table(knn.pred3, test.fall)
(26828+3382)/(26828+3382+4989+3027)
#79.03% When K=3

knn.pred5 = knn(train.x, test.x, train.fall, k=5)
table(knn.pred5, test.fall)
(27360+3249)/(27360+3249+5122+2485)
#80.09% When K=5

