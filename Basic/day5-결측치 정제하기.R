###결측치 정제하기
##빠진 데이터, 이상한 데이터 제거하기
#결측치 찾기
#결측치 만들기
df <- data.frame(sex=c('M','F',NA,'M','F'),
                 score = c(5,4,3,4,NA))
df
#결측치 확인하기
is.na(df)
table(is.na(df))   #결측치 빈도 출력
#변수별로 결측치 확인하기
table(is.na(df$sex))         #sex 결측치 빈도 출력
table(is.na(df$score))       #score 결측치 빈도 출력
#결측치 포함된 상태로 분석
mean(df$score)
sum(df$score)
##결측치 제거하기
library(dplyr)
df %>% 
  filter(is.na(score))    #score가 NA인 데이터만 출력
df %>% 
  filter(!is.na(score))   #score 결측치 제거

df_nomiss <- df %>% 
  filter(!is.na(score))
mean(df_nomiss$score)
##여러 변수 동시에 결측치 없는 데이터 추출하기
#score, sex 결측치 제외
df_nomiss <- df %>% 
                filter(!is.na(score) & !is.na(sex))
df_nomiss
#결측치가 하나라도 있으면 제거하기(분석에 필요한 데이터까지 손실 될 가능성 유의)
df_nomiss2 <- na.omit(df)            #모든 변수에 결측치 없는 데이터 추출
df_nomiss2
#함수의 결측치 제외 기능 이용하기
mean(df$score,na.rm=T)            #결측치 제외하고 평균 산출
sum(df$score,na.rm=T)             #결측치 제외하고 합계 산출
#summarise()에서 na.rm=T사용하기
#결측치 생성
exam <- read.csv('csv_exam.csv')
exam[c(3,8,15),'math'] <- NA
#평균 구하기
exam %>% 
  summarise(mean_math=mean(math))
exam %>% 
  summarise(mean_math=mean(math,na.rm=T))     #결측치 제외하고 평균 산출

###결측치 대체하기
##결측치 대체법
#-대표값(평균, 최빈값 등)으로 일괄 대체
#-통계분석 기법 적용, 예측값 추정해서 대체

###평균값으로 결측치 대체하기
##평균 구하기
mean(exam$math, na.rm=T)     #결측치 제외하고 math 평균 산출
#평균으로 대체하기
exam$math <- ifelse(is.na(exam$math),55,exam$math)      #math가 NA면 55로 대체
table(is.na(exam$math))

####혼자서 해보기