##와인 데이터 불러오기
rawdata <- read.csv('wine.csv',header=T)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

##트레이닝 테스트 셋 나누기
anal <- rawdata
set.seed(2020)
datatotal <- sort(sample(nrow(anal),nrow(anal)*0.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
str(train)
train_x <- train[,1:13]
train_y <- train[,14]
test_x <- test[,1:13]
test_y <- test[,14]

##학습
ctrl <- trainControl(method="repeatedcv",repeats=5)
nbfit <- train(Class~.,
               data=train,
               method="naive_bayes",
               trControl=ctrl,
               preProcess=c('center','scale'),
               metric='Accuracy')
nbfit
plot(nbfit)

##예측
pred <- predict(nbfit,newdata=test)
confusionMatrix(pred,test$Class)

##변수중요도
importance_nb <- varImp(nbfit,scale=F)
plot(importance_nb)                                          #ROC커브의 면적이 넓을수록 중요도 상승
