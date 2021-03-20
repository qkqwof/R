##exam 데이터 파악하기
exam <- read.csv('csv_exam.csv')
head(exam,10)   # 앞에서부터 10개 행
tail(exam,10)   # 끝에서부터 10개 행
View(exam)      # 뷰어 창에서 데이터 확인하기
dim(exam)       # 몇 행 몇 열로 구성되는지 알아보기
str(exam)       # 속성 파악하기
summary(exam)   # 요약 통계량 산출하기

##mpg 데이터 파악하기
#ggplot2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
tail(mpg,10)   # 끝에서부터 10개 행
View(mpg)      # 뷰어 창에서 데이터 확인하기
dim(mpg)       # 몇 행 몇 열로 구성되는지 알아보기
str(mpg)       # 속성 파악하기
summary(mpg)   # 요약 통계량 산출하기

##데이터 변수명 바꾸기
#dplyr 패키지 로드
library(dplyr)
#데이터프레임 생성
df_raw <- data.frame(var1=c(1,2,1),
                     var2=c(2,3,2))
df_raw
#데이터프레임 본사본 만들기
df_new <- data.frame(var1=c(1,2,1),
                     var2=c(2,3,2))
df_new
#변수명 바꾸기
df_new <- rename(df_new,v2=var2)  #var2를 v2로 수정
df_new

##혼자서 해보기
#Q1. ggplot2패키지의 mpg 데이터를 사용할 수 있도록 불러온 뒤 복사본을 만드세요.
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
df_raw <- data.frame(mpg)
df_new <- data.frame(mpg)
head(df_new)

#Q2. 복사본 데이터를 이용해서 cty는 city로, hwy는 highway로 변수명을 수정하세요.
df_new <- rename(df_new, city=cty, highway=hwy)

#Q3. 데이터 일부를 출력해서 변수명이 바뀌었는지 확인해 보세요.
df_new

##파생변수 만들기
#데이터프레임 생성
df <- data.frame(var1=c(4,3,8),
                 var2=c(2,6,1))
df
#파생변수 생성
df$var_sum <- df$var1 + df$var2   # var_sum 파생변수 생성
df
df$var_mean <- df$var_sum / 2     # var_mean 파생변수 생성
df
#mpg 통합 연비 변수 만들기
mpg$total <- (mpg$cty + mpg$hwy) /2    # 통합 연비 변수 생성
head(mpg)

##조건문을 활용해 파생변수 만들기
#가준값 정하기
summary(mpg$total)                 # 요약통계량 생성
hist(mpg$total)                    # 히스토그램 그리기
#조건문으로 합격 판정 변수 만들기
mpg$test <- ifelse(mpg$total >= 20, 'pass','fail')
head(mpg)
#빈도표로 합격 판정 자동차 수 살펴보기
table(mpg$test)
#막대 그래프로 빈도 표현하기
library(ggplot2)
qplot(mpg$test)
#중첩 조건문 활용하기 - 연비 등급 변수 만들기
#total을 기준으로 A,B,C등급 부여
mpg$grade <- ifelse(mpg$total >= 30, 'A',ifelse(mpg$total >=20,'B','C'))
head(mpg,20)
#빈도표, 막대 그래프로 연비 등급 살펴보기
table(mpg$grade)
qplot(mpg$grade)

###분석도전!
###midwset데이터로 분석!
#Q1. ggplot의 midwest데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악
#    하세요.
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)

#Q2. poptotal(전체 인구)을  total로, popaisan(아시아 인구)을 asian으로 변수명을 
#    수정하세요.
midwest <- rename(midwest,total=poptotal,asian=popasian)
midwest

#Q3. total,asian변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를 만
#    들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요.
midwest$percent <- (midwest$asian / midwest$total) * 100 
hist(midwest$percent)

#Q4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 'large', 그 외에는 
#    'small'을 부여하는 파생변수를 만들어 보세요.
mean(midwest$percent)
midwest$asisan_percent <- ifelse(mean(midwest$percent) > 0.4872462,'large','small')
head(midwest)

#Q5. 'large'와 'small'에 해당하는 지역이 얼마나 되는지, 빈도표와 빈도 막대 그래프
#    를 만들어 확인해 보세요.
qplot(midwest$asisan_percent)
