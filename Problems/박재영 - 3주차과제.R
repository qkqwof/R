<<<<<<< HEAD
# Week_3 Quiz
# Date : 2021-01-21
# Description
# 문제 바로 아래에 답안을 작성하여, <<r script 형태>>로 저장 후 제출해주시면 됩니다.
# 과제 제출 기한은 일요일(1/14) 저녁 10까지입니다.

library(dplyr)
library(caret)

## 3주차 과제에서는 2개 데이터를 활용할 예정입니다.
## kc_house : 집과 관련된 정보를 바탕으로 가격을 예측하는 데이터 입니다. (수치형 데이터 예측)
## titanic_train : 타이타닉 호에 탑승한 승객 정보를 바탕으로 생존여부를 예측하는 데이터입니다. (범주형 데이터 예측) 

## 먼저, 본인 컴퓨터에 데이터 파일이 지정된 경로를 통해 데이터를 불러오시기 바랍니다.

## 경로를 정의하는 변수
kc_house_path = "path"
titanic_path = "path"

## 데이터 불러오기
kc_house <- read.csv('kc_house_data.csv', header= TRUE)
titanic <-  read.csv('titanic_train.csv', header= TRUE)

## 범주형 데이터로 바꾸지 않으면 오류가 발생할 수 있습니다. 
titanic$Survived <- as.factor(titanic$Survived)


## 3주차 과제에서는 강의에서 다루지 않은 코드가 일부 존재합니다. 
## 힌트에 참고하실 수 있는 내용을 정리해 두었으니, 이를 활용하여 과제를 진행하시길 바랍니다.



## 1~4번 : kc_house 데이터를 활용하여 문제를 풀어주세요.


## 1. 다음 조건을 충족하는 훈련, 테스트 데이터를 생성해주세요. (10점)
## 조건1) Random seed number : 2021
## 조건2) 8:2의 비율의 Train과 Test 데이터셋 구분 
## 힌트 : "train test split in r" > 다양한 방식이 존재할 수 있으니 구글에서 검색을 통해 문제를 풀어보시는 것을 권장드립니다.

newdata <- as.data.frame(kc_house)
head(newdata)
newdata1 <- newdata %>% 
  select(id,date,bedrooms,bathrooms,sqft_living,sqft_lot,floors,waterfront,view,condition,grade,sqft_above,sqft_basement,yr_built,yr_renovated,zipcode,lat,long,sqft_living15,sqft_lot15,price)
head(newdata1)
set.seed(2021)
totaldata <- sort(sample(nrow(newdata1),nrow(newdata1)* 0.8))
train1 <- newdata1[totaldata,]
test1 <- newdata1[-totaldata,]
train1_x <- train1[,1:20]
train1_y <- train1[,21]
test1_x <- test1[,1:20]
test1_y <- test1[21]

str(train1)
## 2. sqft 관련 변수 5개를 사용하여 Price 예측 하는 다중선형회귀분석 모델을 만들고 결과를 출력해주세요. (10점)
## sqft_oo 관련된 변수 5가지는 면적과 관련된 데이터입니다. (sqft_living, sqft_lot, sqft_above, sqft_living15, sqft_lot15)
## 힌트 
## 1) 가급적 1번에서 생성한 train 데이터를 활용해주세요. (만약 1번을 풀지 못하셨다면 전체 kc_house 데이터를 사용하셔도 무관합니다.)
## 2) 다중선형회귀분석 모델 생성에 사용되는 함수 : lm(formula, data)
## 3) 모델 결과 출력 함수 : summary()

fit <- lm(price~sqft_living+ sqft_lot+ sqft_above+ sqft_living15+ sqft_lot15,data=train1)
summary(fit)


## 3. sqft_living 1개 컬럼에 대해 min-max 스케일링을 적용한 신규 컬럼을 만들어주세요. (10점) 
## 신규 컬럼 이름 : minmax_sqft_living 
## 힌트
## 1) 가급적 1번에서 생성한 train 데이터를 활용해주세요. (만약 1번을 풀지 못하셨다면 전체 kc_house 데이터를 사용하셔도 무관합니다.)
## 2) sqft_living에 대한 min과 max값을 변수로 생성 
## 3) 신규 생성 된 min, max 변수와 mutate 함수를 사용하여 신규 컬럼을 생성

#((x-min(x))/(max(x)-min(x)))
train1_1 <- train1
train1_2 <- train1_1 %>% 
  mutate(minmax_sqft_living = ((sqft_living-min(sqft_living))/(max(sqft_living)-min(sqft_living))))
head(train1_2)


## 4. 2번에서 생성한 train 데이터 모델을 활용하여 test데이터를 예측해주세요.(10점)
## 힌트
## 1) 모델을 바탕으로 데이터를 예측하는 함수 : predict()
pred_test <- predict(fit,newdata=test1)
head(pred_test)


## 5~7번 : titanic_train 데이터를 활용하여 문제를 풀어주세요.


## 5. 다음 조건을 바탕으로 생존여부(Survived)를 측정하는 Rogistic Regression 모델을 생성해주세요. (10점)
## 1) Cross Validation : 5-fold 
## 2) Cross Validation Repeat : 5 
## 3) Boosting 기법이 적용된 Regression Method 사용
## 힌트 
## 1) trainControl Help 문서
## 2) 정상 실행 되거나, missing value 오류가 출력되는 2가지 케이스 모두 정답입니다. 
str(titanic)
newdata2 <- titanic
newdata2 <- newdata2 %>% 
  select(PassengerId,Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Fare,Cabin,Embarked,Survived)
set.seed(2020)
datatotal <- sort(sample(nrow(newdata2),nrow(newdata2)* 0.7))
train <- newdata2[datatotal,]
test <- newdata2[datatotal,]
train_x <- train[,1:11]
train_y <- train[,12]
test_x <- test[,1:11]
test_y <- test[,12]
head(train_x)
head(train_y)
crtl <- trainControl(method = 'repeatedcv',number = 5,repeats=5)
logitfit <- train(Survived~.,
                  data=train,
                  method='LogitBoost',
                  trControl = crtl,
                  metric='Accuracy'
)
logitfit
plot(logitfit)

## 5번 문제에서 null인 데이터가 존재할 경우 모델링이 정상적으로 동작하지 않음을 확인하였습니다.
## 6. null이 존재하는 모든 row를 삭제하는 방식으로 5번 모델링을 재실행해주세요. (10점)
## 힌트
## 1) View 함수를 통해 null로 표시된 데이터는 어떤식으로 보이는지 확인해보시기 바랍니다.
## 2) 1안 : 참고 링크(https://m.blog.naver.com/PostView.nhn?blogId=liberty264&logNo=220992831831&proxyReferer=https:%2F%2Fwww.google.com%2F)
## 3) 2안 : train()함수에서 지원하는 파라미터 사용하기 
table(is.na(train))
summary(train)
table(is.na(train$Age))
train <- na.omit(train)
table(is.na(train))


## 7. 6번에서 생성된 모델에 대한 해석을 작성해주세요. (10점) 

## 해석 : 11개의 Feature를 사용하여 생존 여부를 예측하였음

## 해석 : 31번의 반복이 가장 높은 정확도를 보이고 있음
=======
# Week_3 Quiz
# Date : 2021-01-21
# Description
# 문제 바로 아래에 답안을 작성하여, <<r script 형태>>로 저장 후 제출해주시면 됩니다.
# 과제 제출 기한은 일요일(1/14) 저녁 10까지입니다.

library(dplyr)
library(caret)

## 3주차 과제에서는 2개 데이터를 활용할 예정입니다.
## kc_house : 집과 관련된 정보를 바탕으로 가격을 예측하는 데이터 입니다. (수치형 데이터 예측)
## titanic_train : 타이타닉 호에 탑승한 승객 정보를 바탕으로 생존여부를 예측하는 데이터입니다. (범주형 데이터 예측) 

## 먼저, 본인 컴퓨터에 데이터 파일이 지정된 경로를 통해 데이터를 불러오시기 바랍니다.

## 경로를 정의하는 변수
kc_house_path = "path"
titanic_path = "path"

## 데이터 불러오기
kc_house <- read.csv('kc_house_data.csv', header= TRUE)
titanic <-  read.csv('titanic_train.csv', header= TRUE)

## 범주형 데이터로 바꾸지 않으면 오류가 발생할 수 있습니다. 
titanic$Survived <- as.factor(titanic$Survived)


## 3주차 과제에서는 강의에서 다루지 않은 코드가 일부 존재합니다. 
## 힌트에 참고하실 수 있는 내용을 정리해 두었으니, 이를 활용하여 과제를 진행하시길 바랍니다.



## 1~4번 : kc_house 데이터를 활용하여 문제를 풀어주세요.


## 1. 다음 조건을 충족하는 훈련, 테스트 데이터를 생성해주세요. (10점)
## 조건1) Random seed number : 2021
## 조건2) 8:2의 비율의 Train과 Test 데이터셋 구분 
## 힌트 : "train test split in r" > 다양한 방식이 존재할 수 있으니 구글에서 검색을 통해 문제를 풀어보시는 것을 권장드립니다.

newdata <- as.data.frame(kc_house)
head(newdata)
newdata1 <- newdata %>% 
  select(id,date,bedrooms,bathrooms,sqft_living,sqft_lot,floors,waterfront,view,condition,grade,sqft_above,sqft_basement,yr_built,yr_renovated,zipcode,lat,long,sqft_living15,sqft_lot15,price)
head(newdata1)
set.seed(2021)
totaldata <- sort(sample(nrow(newdata1),nrow(newdata1)* 0.8))
train1 <- newdata1[totaldata,]
test1 <- newdata1[-totaldata,]
train1_x <- train1[,1:20]
train1_y <- train1[,21]
test1_x <- test1[,1:20]
test1_y <- test1[21]

str(train1)
## 2. sqft 관련 변수 5개를 사용하여 Price 예측 하는 다중선형회귀분석 모델을 만들고 결과를 출력해주세요. (10점)
## sqft_oo 관련된 변수 5가지는 면적과 관련된 데이터입니다. (sqft_living, sqft_lot, sqft_above, sqft_living15, sqft_lot15)
## 힌트 
## 1) 가급적 1번에서 생성한 train 데이터를 활용해주세요. (만약 1번을 풀지 못하셨다면 전체 kc_house 데이터를 사용하셔도 무관합니다.)
## 2) 다중선형회귀분석 모델 생성에 사용되는 함수 : lm(formula, data)
## 3) 모델 결과 출력 함수 : summary()

fit <- lm(price~sqft_living+ sqft_lot+ sqft_above+ sqft_living15+ sqft_lot15,data=train1)
summary(fit)


## 3. sqft_living 1개 컬럼에 대해 min-max 스케일링을 적용한 신규 컬럼을 만들어주세요. (10점) 
## 신규 컬럼 이름 : minmax_sqft_living 
## 힌트
## 1) 가급적 1번에서 생성한 train 데이터를 활용해주세요. (만약 1번을 풀지 못하셨다면 전체 kc_house 데이터를 사용하셔도 무관합니다.)
## 2) sqft_living에 대한 min과 max값을 변수로 생성 
## 3) 신규 생성 된 min, max 변수와 mutate 함수를 사용하여 신규 컬럼을 생성

#((x-min(x))/(max(x)-min(x)))
train1_1 <- train1
train1_2 <- train1_1 %>% 
  mutate(minmax_sqft_living = ((sqft_living-min(sqft_living))/(max(sqft_living)-min(sqft_living))))
head(train1_2)


## 4. 2번에서 생성한 train 데이터 모델을 활용하여 test데이터를 예측해주세요.(10점)
## 힌트
## 1) 모델을 바탕으로 데이터를 예측하는 함수 : predict()
pred_test <- predict(fit,newdata=test1)
head(pred_test)


## 5~7번 : titanic_train 데이터를 활용하여 문제를 풀어주세요.


## 5. 다음 조건을 바탕으로 생존여부(Survived)를 측정하는 Rogistic Regression 모델을 생성해주세요. (10점)
## 1) Cross Validation : 5-fold 
## 2) Cross Validation Repeat : 5 
## 3) Boosting 기법이 적용된 Regression Method 사용
## 힌트 
## 1) trainControl Help 문서
## 2) 정상 실행 되거나, missing value 오류가 출력되는 2가지 케이스 모두 정답입니다. 
str(titanic)
newdata2 <- titanic
newdata2 <- newdata2 %>% 
  select(PassengerId,Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Fare,Cabin,Embarked,Survived)
set.seed(2020)
datatotal <- sort(sample(nrow(newdata2),nrow(newdata2)* 0.7))
train <- newdata2[datatotal,]
test <- newdata2[datatotal,]
train_x <- train[,1:11]
train_y <- train[,12]
test_x <- test[,1:11]
test_y <- test[,12]
head(train_x)
head(train_y)
crtl <- trainControl(method = 'repeatedcv',number = 5,repeats=5)
logitfit <- train(Survived~.,
                  data=train,
                  method='LogitBoost',
                  trControl = crtl,
                  metric='Accuracy'
)
logitfit
plot(logitfit)

## 5번 문제에서 null인 데이터가 존재할 경우 모델링이 정상적으로 동작하지 않음을 확인하였습니다.
## 6. null이 존재하는 모든 row를 삭제하는 방식으로 5번 모델링을 재실행해주세요. (10점)
## 힌트
## 1) View 함수를 통해 null로 표시된 데이터는 어떤식으로 보이는지 확인해보시기 바랍니다.
## 2) 1안 : 참고 링크(https://m.blog.naver.com/PostView.nhn?blogId=liberty264&logNo=220992831831&proxyReferer=https:%2F%2Fwww.google.com%2F)
## 3) 2안 : train()함수에서 지원하는 파라미터 사용하기 
table(is.na(train))
summary(train)
table(is.na(train$Age))
train <- na.omit(train)
table(is.na(train))


## 7. 6번에서 생성된 모델에 대한 해석을 작성해주세요. (10점) 

## 해석 : 11개의 Feature를 사용하여 생존 여부를 예측하였음

## 해석 : 31번의 반복이 가장 높은 정확도를 보이고 있음
>>>>>>> c3a0582dd9af788e0400bb6bdf7b8dbae6a9ad9d
