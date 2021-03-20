####와인 데이터 불러오기
library(caret)
rawdata <- read.csv('D:/Rproject/startr/wine.csv',header=T)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

####트레이닝/테스트셋 나누기(7:3)
anal <- rawdata
set.seed(2020)
datatotal <- sort(sample(nrow(anal),nrow(anal)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
str(train)
train_x <- train[,1:13]
train_y <- train[,14]
test_x <- test[,1:13]
test_y <- test[,14]

####선형 서포트 벡터 머신
ctrl <- trainControl(method='repeatedcv',repeats=5)
svm_linear_fit <- train(Class~.,data=train,
                        method='svmLinear',
                        trControl=ctrl,
                        preProcess=c('center','scale'),
                        metric='Accuracy')
svm_linear_fit

####예측하기
pred <- predict(svm_linear_fit,newdata=test)
confusionMatrix(pred,test$Class)

####변수중요도
importance_linear <- varImp(svm_linear_fit,scale=F)
plot(importance_linear)

####비선형 서포트 벡터머신
ctrl <- trainControl(method='repeatedcv',repeats=5)
svm_poly_fit <- train(Class~.,data=train,
                        method='svmPoly',
                        trControl=ctrl,
                        preProcess=c('center','scale'),
                        metric='Accuracy')
svm_poly_fit
plot(svm_poly_fit)

####예측하기
pred <- predict(svm_poly_fit,newdata=test)
confusionMatrix(pred,test$Class)
