##데이터 전처리-원하는 형태로 데이터 가공하기
#조건에 맞는 데이터만 추출하기
#데이터 준비
library(dplyr)
exam <- read.csv('csv_exam.csv')
exam
#exam에서 class가 1인 경우만 추출하여 출력
exam %>%
  filter(class==1)
#1반이 아닌 경우
exam %>% 
  filter(class != 1)
#초과, 미만, 이상, 이하 조건 걸기
#수학 점수가 50점을 초과한 경우
exam %>% 
  filter(math > 50)
#영어 점수가 80점 이상인 경우
exam %>% 
  filter(english >= 80)
#여러 조건을 충족하는 행 추출하기
#1반 이면서 수학 점수가 50점 이상인 경우
exam %>% 
  filter(class == 1 & math >= 50)
#2반 이면서 영어 점수가 80점 이상인 경우
exam %>% 
  filter(class == 2 & english >=80)
#여러 조건 중 하나 이상 충족하는 행 추출하기
#수학 점수가 90점 이상이거나, 영어점수가 90점 이상인 경우
exam %>% 
  filter(math >= 90 || english >= 90)
#영어점수가 90점 미만이거나 과학점수가 50점 미만인 경우

#1,3,5반에 해당되면 추출

#%in% 기호 이용하기
#1,3,5반에 해당되면 추출
exam %>% 
  filter(class %in% c(1,3,5))
#추출한 행으로 데이터 만들기
#class가 1인 행 추출, class1에 할당
class1 <- exam %>% 
            filter(class == 1)
class1
#class가 2인 행 추출, class2에 할당
class2 <- exam %>% 
            filter(class == 2)
#1반 수학 점수 평균 구하기
mean(class1$math)
#2반 수학 점수 평균 구하기
mean(class2$math)

###혼자서 해보기
#mpg 데이터를 이용해 분석 문제를 해결해 보세요.
#Q1. 자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다. displ(배기량)
#    이 4이하인 자동차와 5이상인 자동차 중 어떤 자동차의 hwy(고속도로 연비)가 
#    평균적으로 더 높은지 알아보세요.
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
mpg_a <- mpg %>%  filter(displ>=5)
mpg_b <- mpg %>%  filter(displ<=4)
mean(mpg_a$hwy)
mean(mpg_b$hwy)

#Q2. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다. 'audi'와 'toyota'
#    중에서 어느 manufacturer(자동차 제조 회사)의 cty(도시 연비)가 평균적으로
#    더 높은지 알아보세요.
mpg_audi <- mpg %>% 
                filter(manufacturer == 'audi')
mpg_toyota <- mpg %>% 
                filter(manufacturer == 'toyota')
mean(mpg_audi$cty)
mean(mpg_toyota$cty)

#Q3. 'chevrolet','ford','honda' 자동차의 고속도로 연비 평균을 알아보려고 합니다.
#    이 회사들의 자동차를 추출한 뒤 hwy 전체 평균을 구해보세요.
mpg_chevrolet <- mpg %>% 
                    filter(manufacturer=='chevrolet')
mpg_ford <- mpg %>% 
                filter(manufacturer=='ford')
mpg_honda <- mpg %>% 
                filter(manufacturer=='honda')
mean(mpg_chevrolet$hwy)
mean(mpg_ford$hwy)
mean(mpg_honda$hwy)