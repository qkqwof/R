###Logistic Regression
##데이터 불러오기
rawdata <- read.csv('heart.csv',header=T)
str(rawdata)

##타겟 클래스 범주화
rawdata$target <- as.factor(rawdata$target)
unique(rawdata$target)

##연속형 -> 표준화, 범주형 -> 범주화
#연속형 독립변수 표준화
rawdata$age <- scale(rawdata$age)
rawdata$trestbps <- scale(rawdata$trestbps)
rawdata$chol <- scale(rawdata$chol)
rawdata$thalach <- scale(rawdata$thalach)
rawdata$oldpeak <- scale(rawdata$oldpeak)
rawdata$slope <- scale(rawdata$slope)

##범주형 독립변수 as.factor
newdata <- rawdata
factorvar <- c('sex','cp','fbs','restecg','exang','ca','thal')
newdata[,factorvar] = lapply(newdata[,factorvar],factor)

##트레이닝 테스트 셋 나누기
set.seed(2020)
datatotal <- sort(sample(nrow(newdata),nrow(newdata)* 0.7))
train <- newdata[datatotal,]
test <- newdata[datatotal,]
train_x <- train[,1:12]
train_y <- train[,13]
test_x <- test[,1:12]
test_y <- test[,13]

##LogitBoost
crtl <- trainControl(method = 'repeatedcv',repeats=5)
logitfit <- train(target~.,
                  data=train,
                  method='LogitBoost',
                  trControl = ctrl,
                  metric='Accuracy'
                  )
logitfit
plot(logitfit)

##예측하기
pred <- predict(logitfit,newdata=test)
confusionMatrix(pred,test$target)

##변수중요도
importance_logit <- varImp(logitfit,scale=F)              #스케일 T -> 0부터 100까지 True값
plot(importance_logit)
