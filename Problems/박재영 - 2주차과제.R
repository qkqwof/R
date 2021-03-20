<<<<<<< HEAD
# Week_2 Quiz
# Date : 2021-01-14
# Description
# [R Script Encoding Error] : File > Reopen with Encoding > 'UTF-8' > OK
# 문제 바로 아래에 답안을 작성하여, <<r script 형태>>로 저장 후 제출해주시면 됩니다.
# 답안인 코드만 작성하시면 됩니다. 코드 실행 결과 포함 X
# 과제 제출 기한은 일요일(1/17) 저녁 10시 까지입니다.

library(dplyr)

# (5점) 1-(1). 2주차 과제 외부데이터에 배포된 kc_house_data.csv를 다운받고 불러오세요. 
# 데이터 출처 : https://www.kaggle.com/harlfoxem/housesalesprediction
data <- read.csv('kc_house_data.csv',header = T)
head(data)

# (5점) 1-(2). 데이터를 검토할 수 있는 함수를 1개 이상 작성하세요.
# Hint. Part3 >  07. Ch 08. 프로젝트를 통해 R 프로그래밍에 익숙해지기 - 01. 한국복지패널 데이터를 활용한 한국인의 삶 분석 참고
dim(data)
str(data)
head(data)
summary(data)
View(data)

# (10점) 2. kc_house 데이터에는 층 수를 나타내는 floors 변수가 있습니다. floors 변수의 각 층별 빈도를 구하세요.
floors <- data %>% 
  group_by(floors) %>% 
  summarise(count = n())
floors

# (10점) 3. floors 변수가 1이면 'low', 1보다 크면 'high'로 구분하는 변수 'floor_level'을 만드세요.
# Hint : ifelse 함수와 $를 이용하여 해결해보세요.
summary(data$floors)
floor_level <- data %>% 
  mutate(floor_level = ifelse(data$floors>1,'high','low'))
head(floor_level)

# (10점) 4. 3에서 생성한 floor_level에 따른 평균 집 값을 출력하는 함수를 작성하세요. 집 값에 관한 변수는 price입니다.
# Hint : %>%와 dplyr에서 제공하는 함수를 이용하여 해결해보세요.
price <- data %>% 
  group_by(data$floor_level) %>% 
  summarise(mean_price=mean(price))
price

# (10점) 5. grade가 4인 집과 13인 집의 평균 집 값을 비교하고 싶습니다. 
# grade가 4인 집은 group1, 13인 집은 group2라는 새로운 이름의 데이터 프레임을 생성하세요. 이 때, 변수는 grade와 price만 추출하세요.
# Hint. Data slicing과 관련된 과제입니다. dplyr의 filter, select를 이용하거나 데이터에 직접 접근하여 해결해보세요.
group1 <- data %>% 
  filter(grade == 4) %>% 
  select(grade,price)
group2 <- data %>% 
  filter(grade==13) %>% 
  select(grade,price)
group1
group2

# (5점) 6-(1). 5에서 생성한 데이터프레임으로 grade가 4인 집과 13인 집에 대한 아래의 가설을 검정하려고 합니다.
# 귀무가설 : grade가 4인 집과 13인 집의 가격은 동일하다.
# 대립가설 : grade가 4인 집과 13인 집의 가격은 다르다.
# T검정을 이용하여 가설검정을 진행하세요. (단, 정규성과 등분산성을 만족한다고 가정합니다.)

# 정규성 검정
shapiro.test(group1[,2])
qqnorm(group1[,2])
qqline(group1[,2])
shapiro.test(group2[,2])      #정규성을 만족한다고 가정했으니까 그냥 넘어감.
qqnorm(group2[,2])
qqline(group2[,2]) 

#등분산성 검정
var.test(group1[,2],group2[,2])

#T-test
t.test(group1[,2],group2[,2],alternative = 'less',var.equal = T)
#등분산성 만족한다고 해서 T로 놓음

# (5점) 6-(2). 위 결과를 해석하세요.
# 해석 :  p-value가 4.698e-13 으로 유의수준 0.05보다 작으므로 귀무가설을 기각하고 대립가을 채택한다!  (빈칸을 작성하여 문장을 완성하세요.)


# r에서는 문자열의 위치에 따라 추출할 수 있는 substr함수가 있습니다. 아래 코드를 실행하여 month 라는 변수를 만들고 아래 7번 문제를 해결하세요.(참고: substr함수의 수행 결과는 character형태임)
# 주의 : kc_house는 1-(1)에서 지정한 데이터프레임 이름으로,다를 경우 수정하여 코드를 실행하세요.
data$month <- substr(data$date, 5,6)


# (5점) 7-(1). 월 별로 평균 집값에 차이가 있는지 알고 싶습니다. 
# 귀무가설 : 월 별로 평균 집 값에 차이가 없다.
# 대립가설 : 월 별로 평균 집 값에 차이가 있다.
# ANOVA 검정을 시행하세요. (단, 정규성과 등분산성을 만족한다고 가정합니다.)
raw_anova <- data %>% 
  select(month,price)
head(raw_anova)

## 그룹 나누기
group01 <-raw_anova[raw_anova$month=='01',1:2]
group02 <- raw_anova[raw_anova$month=='02',1:2]
group03 <-raw_anova[raw_anova$month=='03',1:2]
group04 <-raw_anova[raw_anova$month=='04',1:2] 
group05 <- raw_anova[raw_anova$month=='05',1:2]
group06 <-raw_anova[raw_anova$month=='06',1:2]
group07 <-raw_anova[raw_anova$month=='07',1:2] 
group08 <- raw_anova[raw_anova$month=='08',1:2]
group09 <-raw_anova[raw_anova$month=='09',1:2]
group10 <-raw_anova[raw_anova$month=='10',1:2] 
group11 <- raw_anova[raw_anova$month=='11',1:2]
group12 <-raw_anova[raw_anova$month=='12',1:2]
mean(group01[,2])
mean(group02[,2])
mean(group03[,2])
mean(group04[,2])
mean(group05[,2])
mean(group06[,2])
mean(group07[,2])
mean(group08[,2])
mean(group09[,2])
mean(group10[,2])
mean(group11[,2])
mean(group12[,2])
###정규성과 등분산성을 만족한다고 가정해서 두 개의 검정은 생략!!

##ANOVA 검정
RAWANOVA <- aov(price~month,data=raw_anova)
summary(RAWANOVA) 

# (5점) 7-(2). 7-(1) 결과를 해석하세요.
# 주의 : summary 함수를 사용해야 p-value를 확인할 수 있습니다.

# 해석 : p-value가 0.000287으로 유의수준 0.05보다 작으므로 귀무가설을 기각하고 대립가설을 채택한다!  (빈칸을 작성하여 문장을 완성하세요.)
=======
# Week_2 Quiz
# Date : 2021-01-14
# Description
# [R Script Encoding Error] : File > Reopen with Encoding > 'UTF-8' > OK
# 문제 바로 아래에 답안을 작성하여, <<r script 형태>>로 저장 후 제출해주시면 됩니다.
# 답안인 코드만 작성하시면 됩니다. 코드 실행 결과 포함 X
# 과제 제출 기한은 일요일(1/17) 저녁 10시 까지입니다.

library(dplyr)

# (5점) 1-(1). 2주차 과제 외부데이터에 배포된 kc_house_data.csv를 다운받고 불러오세요. 
# 데이터 출처 : https://www.kaggle.com/harlfoxem/housesalesprediction
data <- read.csv('kc_house_data.csv',header = T)
head(data)

# (5점) 1-(2). 데이터를 검토할 수 있는 함수를 1개 이상 작성하세요.
# Hint. Part3 >  07. Ch 08. 프로젝트를 통해 R 프로그래밍에 익숙해지기 - 01. 한국복지패널 데이터를 활용한 한국인의 삶 분석 참고
dim(data)
str(data)
head(data)
summary(data)
View(data)

# (10점) 2. kc_house 데이터에는 층 수를 나타내는 floors 변수가 있습니다. floors 변수의 각 층별 빈도를 구하세요.
floors <- data %>% 
  group_by(floors) %>% 
  summarise(count = n())
floors

# (10점) 3. floors 변수가 1이면 'low', 1보다 크면 'high'로 구분하는 변수 'floor_level'을 만드세요.
# Hint : ifelse 함수와 $를 이용하여 해결해보세요.
summary(data$floors)
floor_level <- data %>% 
  mutate(floor_level = ifelse(data$floors>1,'high','low'))
head(floor_level)

# (10점) 4. 3에서 생성한 floor_level에 따른 평균 집 값을 출력하는 함수를 작성하세요. 집 값에 관한 변수는 price입니다.
# Hint : %>%와 dplyr에서 제공하는 함수를 이용하여 해결해보세요.
price <- data %>% 
  group_by(data$floor_level) %>% 
  summarise(mean_price=mean(price))
price

# (10점) 5. grade가 4인 집과 13인 집의 평균 집 값을 비교하고 싶습니다. 
# grade가 4인 집은 group1, 13인 집은 group2라는 새로운 이름의 데이터 프레임을 생성하세요. 이 때, 변수는 grade와 price만 추출하세요.
# Hint. Data slicing과 관련된 과제입니다. dplyr의 filter, select를 이용하거나 데이터에 직접 접근하여 해결해보세요.
group1 <- data %>% 
  filter(grade == 4) %>% 
  select(grade,price)
group2 <- data %>% 
  filter(grade==13) %>% 
  select(grade,price)
group1
group2

# (5점) 6-(1). 5에서 생성한 데이터프레임으로 grade가 4인 집과 13인 집에 대한 아래의 가설을 검정하려고 합니다.
# 귀무가설 : grade가 4인 집과 13인 집의 가격은 동일하다.
# 대립가설 : grade가 4인 집과 13인 집의 가격은 다르다.
# T검정을 이용하여 가설검정을 진행하세요. (단, 정규성과 등분산성을 만족한다고 가정합니다.)

# 정규성 검정
shapiro.test(group1[,2])
qqnorm(group1[,2])
qqline(group1[,2])
shapiro.test(group2[,2])      #정규성을 만족한다고 가정했으니까 그냥 넘어감.
qqnorm(group2[,2])
qqline(group2[,2]) 

#등분산성 검정
var.test(group1[,2],group2[,2])

#T-test
t.test(group1[,2],group2[,2],alternative = 'less',var.equal = T)
#등분산성 만족한다고 해서 T로 놓음

# (5점) 6-(2). 위 결과를 해석하세요.
# 해석 :  p-value가 4.698e-13 으로 유의수준 0.05보다 작으므로 귀무가설을 기각하고 대립가을 채택한다!  (빈칸을 작성하여 문장을 완성하세요.)


# r에서는 문자열의 위치에 따라 추출할 수 있는 substr함수가 있습니다. 아래 코드를 실행하여 month 라는 변수를 만들고 아래 7번 문제를 해결하세요.(참고: substr함수의 수행 결과는 character형태임)
# 주의 : kc_house는 1-(1)에서 지정한 데이터프레임 이름으로,다를 경우 수정하여 코드를 실행하세요.
data$month <- substr(data$date, 5,6)


# (5점) 7-(1). 월 별로 평균 집값에 차이가 있는지 알고 싶습니다. 
# 귀무가설 : 월 별로 평균 집 값에 차이가 없다.
# 대립가설 : 월 별로 평균 집 값에 차이가 있다.
# ANOVA 검정을 시행하세요. (단, 정규성과 등분산성을 만족한다고 가정합니다.)
raw_anova <- data %>% 
  select(month,price)
head(raw_anova)

## 그룹 나누기
group01 <-raw_anova[raw_anova$month=='01',1:2]
group02 <- raw_anova[raw_anova$month=='02',1:2]
group03 <-raw_anova[raw_anova$month=='03',1:2]
group04 <-raw_anova[raw_anova$month=='04',1:2] 
group05 <- raw_anova[raw_anova$month=='05',1:2]
group06 <-raw_anova[raw_anova$month=='06',1:2]
group07 <-raw_anova[raw_anova$month=='07',1:2] 
group08 <- raw_anova[raw_anova$month=='08',1:2]
group09 <-raw_anova[raw_anova$month=='09',1:2]
group10 <-raw_anova[raw_anova$month=='10',1:2] 
group11 <- raw_anova[raw_anova$month=='11',1:2]
group12 <-raw_anova[raw_anova$month=='12',1:2]
mean(group01[,2])
mean(group02[,2])
mean(group03[,2])
mean(group04[,2])
mean(group05[,2])
mean(group06[,2])
mean(group07[,2])
mean(group08[,2])
mean(group09[,2])
mean(group10[,2])
mean(group11[,2])
mean(group12[,2])
###정규성과 등분산성을 만족한다고 가정해서 두 개의 검정은 생략!!

##ANOVA 검정
RAWANOVA <- aov(price~month,data=raw_anova)
summary(RAWANOVA) 

# (5점) 7-(2). 7-(1) 결과를 해석하세요.
# 주의 : summary 함수를 사용해야 p-value를 확인할 수 있습니다.

# 해석 : p-value가 0.000287으로 유의수준 0.05보다 작으므로 귀무가설을 기각하고 대립가설을 채택한다!  (빈칸을 작성하여 문장을 완성하세요.)
>>>>>>> c3a0582dd9af788e0400bb6bdf7b8dbae6a9ad9d
