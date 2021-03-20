###집단 별로 데이터 요약하기
#요약하기
#class별로 분리 후 math 평균 산출
exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math))
#여러 요약통계량 한 번에 산출하기
exam %>% 
  group_by(class) %>%                        #class별로 분리
  summarise(mean_math= mean(math),           #math 평균     
            sum_math= sum(math),             #math 합계
            median_math = median(math),      #math 중앙값
            n = n())                         #학생 수(개별 행의 개수 세는 것!)
#각 집단별로 다시 집단 나누기
mpg %>% 
  group_by(manufacturer,drv) %>%             #회사별, 구성방식별 분리
  summarise(mean_cty=mean(cty)) %>%          #cty 평균 산출
  head(10)                                   #일부 출력

#혼자서 해보기
#Q1. mpg데이터의 class는 'suv','compact' 등 자동차를 특징에 따라 일곱 종류로 
#    분류한 변수입니다. 어떤 차종의 연비가 높은지 비교해보려고 합니다. class별
#    cty 평균을 구해보세요.
library(dplyr)
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

#Q2. 앞 문제의 출력 결과는 class값 알파벳 순으로 정렬되어 있습니다. 어떤 차종의
#    도시 연비가 높은지 쉽게 알아볼 수 있도록 cty평균이 높은 순으로 정렬해 출력
#    하세요.
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(10)

#Q3. 어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다.
#    hwy평균이 가장 높은 회사 세 곳을 출력하세요.
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy=mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

#Q4. 어떤 회사에서 'compact(경차)' 차종을 가장 많이 생산하는지 알아보려고 합니다.
#    각 회사별 'compact'차종 수를 내림차순으로 정렬해 출력하세요.
mpg %>% 
  filter(class == 'compact') %>% 
  group_by(manufacturer) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(10)

###데이터 합치기
#데이터 생성
#중간고사 데이터  생성
test1 <- data.frame(id=c(1,2,3,4,5),
                    midterm=c(60,80,70,90,85))
#기말고사 데이터 생성
test2 <- data.frame(id=c(1,2,3,4,5),
                    final=c(70,83,65,95,80))
#id기준으로 합치기
total <- left_join(test1,test2,by='id')
total
##다른 데이터 활용해 변수 추가하기
#반별 담임교사 명단 생성
name <- data.frame(class=c(1,2,3,4,5),
                   teacher = c('kim','lee','park','choi','jung'))
name_exam <- left_join(exam,name,by='class')
name_exam
##세로로 합치기
#데이터 생성
#학생 1~5번 시험 데이터 생성
group_a <- data.frame(id=c(1,2,3,4,5),
                      test=c(60,80,70,90,85))
#학생 6~10번 시험 데이터 생성
group_b <- data.frame(id=c(1,2,3,4,5),
                    test=c(70,83,65,95,80))
#세로로합치기
group <- bind_rows(group_a,group_b)
group

#혼자 해보기##
#mpg데이터의 f1변수는 자동차에 사용하는 연료를 의미합니다. 아래는 자동차 연료별
#가격을 나타낸 표입니다.
fuel <- data.frame(f1=c('c','d','e','p','r'),
                   price_f1=c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = F)
fuel

#Q1. mpg데이터에는 연료 종류를 나타낸 f1변수는 있지만 연료 가격을 나타낸 변수는 
#    없습니다. 위에서 만든 fuel데이터를 이용해서 mpg데이터에 price_f1(연료 가격)
#    변수를 추가하세요.

#Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model,f1,price_f1변수를 추출
#    해 앞부분 5행을 출력해 보세요.

##분석 도전 2 ##