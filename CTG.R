

setwd("C:/Users/Puru/Desktop/CTG")
CTG.data <- read.csv("C:/Users/Puru/Desktop/CTG/Cardiotoctg.csv")
CTG.data <- read.csv(file.choose())
str(CTG.data)
CTG.data$NSP <- as.factor(CTG.data$NSP)

prop.table(table((CTG.data$NSP)))

CTG.data$NSP <- relevel(CTG.data$ NSP, ref = '1')

set.seed(123)
ind <- createDataPartition(CTG.data$NSP, times = 1, p = 0.7, list = F)
train <- CTG.data[ind,]
test <- CTG.data[-ind,]
prop.table(table((train$NSP)))

#plot
hist(CTG.data$Mean)
ggplot(CTG.data, aes(x= Mean)) + geom_histogram()


#Model

library(caret)
library(psych)
library(nnet)
library(quantmod)
library(PerformanceAnalytics)

Model <- multinom(NSP~.-Nzeros- Width, data = train)
pairs.panels(train[,-16,-12])

heatmap(as.matrix(train[,-16,-12]))
summary(Model)
z_test <- summary(Model)$coefficients/summary(Model)$standard.errors

(1- pnorm(abs(z_test), 0, 1))*2

pred <- predict(Model, train)

confusionMatrix(pred, train$NSP)


#prediction on test data                 
pred <- predict(Model, test)

confusionMatrix(pred, test$NSP)

######################################################
#prediction on test data using random Forest
library(randomForest)
model2 <- randomForest(NSP~., data = train)
pred2 <- predict(model2, test)
confusionMatrix(pred2, test$NSP)


##########################################################

set.seed(1234)
cvcontrol <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 2,
                          allowParallel = T)

bag <- train(NSP~.,
             data = train,
             method = "treebag",
             trControl = cvcontrol,
             importance =T)

plot(varImp(bag))
varImpPlot(model2)

######################################################

bag <- train(NSP~.,
             data = train,
             method = "rf",
             trControl = cvcontrol,
             importance =T)

 pred3 <- predict(bag, test)
confusionMatrix(pred3, test$NSP)

######################################################

library(xgboost)
set.seed(1234)
Boosting <- train(NSP~.,
                  data = train,
                  method = "xgbTree",
                  trControl = cvcontrol,
                  tuneGrid =expand.grid(nrounds = 600,
                                        max_depth =5,
                                        eta = 0.2,
                                        gamma    = 2,
                                        colsample_bytree =1,
                                        min_child_weight =1,
                                        subsample =1))

pred4 <- predict(bag, test)
confusionMatrix(pred4, test$NSP)
