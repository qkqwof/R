##Caret 패키지 설치
install.packages('caret',dependencies = TRUE)
library(caret)

##Caret 함수 소개
#trainControl() -> 데이터 훈련 과정의 파라미터 설정
trainControl(
          method = 'repeatedcv'             #cross-validation 반복
          number = 10                       #훈련 데이터 fold개수
          repeats = 5                       #cv반복횟수
)

#expand.grid() -> 모든 벡터 혹은 인자(factor) 조합인 데이터 프레임 생성
expand.grid(k = 1:10)              #k는 1부터 10까지 고려

#train() -> 머신러닝 알고리즘을 이용해 데이터 학습을 통한 모델 생성
train(
  Class~.,                                #타켓~피쳐    .은 다 넣는 것
  data = train,                           
  method = "knn",                         #사용하고 싶은 머신러닝 방법
  trControl = trainControl(),             #학습방법
  preProcess = c('center','scale'),       #표준화:평균을 뺴고 표준편차로 나눔
  tuneGrid = expand.grid(k=1:10),         #튜닝 파라미터 값 목록
  metric = "Accuracy"                     #모형 평가 방식
)