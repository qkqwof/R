###필요한 변수만 추출하기
#math(칼럼) 추출
exam %>% 
  select(math)
#여러 변수 추출하기
exam %>% 
  select(class,math,english)
#변수 제외하기
exam %>% 
  select(-math, -english)
##dplyr 함수 조합하기
#class가 1인 행만 추출한 다음 english 추출
exam %>% 
  filter(class == 1) %>% 
  select(english)
#일부만 출력하기
exam %>% 
  select(id,math) %>%          #id, math 추출하기 
  head(10)                     #앞부분 10행까지 추출하기

###혼자서 해보기
#mpg 데이터를 이용해서 분석 문제를 해결해보세요.
#Q1. mpg데이터는 11개 변수로 구성되어 있습니다. 이 중 일부만 추출해서 분석에 
#    활용하려고 합니다. mpg데이터에서 class(자동차 종류), cty(도시 연비) 변수를
#    추출해 새로운 데이터를 만드세요. 새로 만든 데이터의 일부를 출력해서 두 변수
#    로만 구성되어 있는지 확인하세요.
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)
mpg %>% 
  select(class,cty) %>% 
  head(10)

#Q2. 자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다. 앞에서 추출한
#    데이터를 이용해서 class(자동차 종류)가 'suv'인 자동차와 'compact'인 자동차
#    중 어떤 자동차의 cty(도시 연비)가 더 높은지 알아보세요.
mpg_suv <- mpg %>%
              filter(class=='suv') %>% 
              select(class,cty)
mpg_compact <- mpg %>% 
                  filter(class=='compact') %>% 
                  select(class,cty)
mean(mpg_suv$cty)
mean(mpg_compact$cty)

###데이터를 정렬하기
#오름차순으로 정렬하기
exam %>% 
  arrange(math)
#내림차순으로 정렬하기
exam %>% 
  arrange(desc(math))
#정렬 기준 변수 여러개 지정
exam %>% 
  arrange(class,math)

###혼자서 해보기
#Q1.'audi'에서 생산한 자동차 중에 어떤 자동차 모델의 hwy(고속도로 연비)가 높은지
#   알아보려고 합니다. 'audi'에서 생산한 자동차 중 hwy가 1~5위에 해당하는 자동차
#   의 데이터를 출력하세요.
mpg %>% 
  filter(manufacturer=='audi') %>% 
  arrange(desc(hwy)) %>% 
  head(5)

###파생변수 추가하기
#총합 변수 추가, 일부 추출
exam %>% 
  mutate(total = math + english + science) %>% 
  head
#여러 파생변수 한 번에 추가하기
exam %>% 
  mutate(total= math + english + science,
         mean = (math + english + science) / 3) %>% 
  head
#mutate()에 ifelse() 적용하기
exam %>% 
  mutate(test=ifelse(science>=60,'pass','fail')) %>% 
  head
#추가한 변수를 dplyr 코드에 바로 활용하기
exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head

###혼자서 해보기
#mpg데이터는 연비를 나타내는 변수가 hwy(고속도로 연비),cty(도시 연비) 두 종류로
#분리되어 있습니다. 두 변수를 각각 활용하는 대신 하나의 통합 연비 변수를 만들어
#분석하려고 합니다.
#Q1. mpg데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.
mpg %>% 
  mutate(plus = cty + hwy) %>% 
  head

#Q2. 앞에서 만든 '합산 연비 변수'를 2로 나눠 평균 연비 변수'를 출력하세요.
mpg %>% 
  mutate(mean_plus = (cty + hwy) / 2) %>% 
  head

#Q3. '평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요.
mpg %>% 
  mutate(mean_plus = (cty + hwy) / 2) %>% 
  arrange(desc(mean_plus)) %>% 
  head(3)

#Q4. 1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 출력하세요.
#    데이터는 복사본 대신 mpg원본을 이용하세요.
mpg %>% 
  mutate(plus = cty +hwy,
         mean_plus = plus / 2) %>%
  arrange(mean_plus) %>% 
  head(3)
