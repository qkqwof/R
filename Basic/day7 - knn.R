###데이터 소개
##와인 데이터
#데이터 불러오기
rawdata <- read.csv('wine.csv',header=T)
rawdata$Class <- as.factor(rawdata$Class)                 #타켓 변수에 접근을 해서 factor로 변경(숫자가 아닌 범주형이라서 factor로 변경)
str(rawdata)

#트레이닝- 테스트 데이터 분할
anal <- rawdata
set.seed(2020)                                             #시드 설정: 랜덤으로 뽑지만 비교할 때 유용하게 할 수 있음
datatotal <- sort(sample(nrow(anal),nrow(anal)*0.7))       #nrow():데이터 행의 수, sort(): 오름차순 정렬, sample(a,b): 1부터 a까지 숫자 중에 b개 추출
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
train_x <- train[,1:13]
train_y <- train[,14]
test_x <- test[,1:13]
test_y <- test[,14]

#모형 학습
ctrl <- trainControl(method='repeatedcv',number=10,repeats=5)
customGrid <- expand.grid(k=1:10)
knnfit <- train(Class~.,
                data=train,
                method='knn',
                trControl = ctrl,
                preProcess = c('center','scale'),
                tuneGrid = customGrid,
                metric="Accuracy"
)
knnfit
plot(knnfit)

#예측
pred_test <- predict(knnfit,newdata=test)
confusionMatrix(pred_test,test$Class)

#변수중요도
importance_knn <- varImp(knnfit,scale=F)
plot(importance_knn)
