###와인 데이터 불러오기
##install.packages('caret',dependencies=T)
library(caret)
rawdata <- read.csv('D:/Rproject/startr/wine.csv',header=T)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

###트레이닝/테스트 데이터셋 분리(7:3)
anal <- rawdata
set.seed(2020)
datatotal <- sort(sample(nrow(anal),nrow(anal)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
train_X <- train[,1:13]
train_y <- train[,14]
test_X <- test[,1:13]
test_y <- test[,14]

###Decision Tree- 'tree'패키지 설치
install.packages('tree')
library(tree)

###기본적인 트리 작성
treeRaw <- tree(Class~.,data=train)
plot(treeRaw)
text(treeRaw)

###Cross Validation
cv_tree <- cv.tree(treeRaw,FUN=prune.misclass)
plot(cv_tree)

###가지치기(pruning)
prune_tree <- prune.misclass(treeRaw,best=4)
plot(prune_tree)
text(prune_tree,pretty=0)

###예측하기
pred <- predict(prune_tree,test,type='class')
confusionMatrix(pred,test$Class)


###Random Forest
##install.packages('caret',dependencies=T)
library(caret)

###트레이닝/테스트 데이터셋 분리(7:3)
anal <- rawdata
set.seed(2020)
datatotal <- sort(sample(nrow(anal),nrow(anal)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
train_X <- train[,1:13]
train_y <- train[,14]
test_X <- test[,1:13]
test_y <- test[,14]

###random Forest 모델 피팅
ctrl <- trainControl(method='repeatedcv',repeats=5)
rffit <- train(Class~.,
               data=train,
               method = 'rf',
               trControl = ctrl,
               preProcess = c('center','scale'),
               metric='Accuracy')
rffit
plot(rffit)

###예측하기
pred_test <- predict(rffit,newdata=test)
confusionMatrix(pred_test,test$Class)

###변수중요도
importance_rf <- varImp(rffit,scale=F)
plot(importance_rf)
