####여러 집단 평균 차이 검정
###ANOVA검정(양측검정)
##데이터 불러오기
raw_anova <- read.csv('htest04.csv',header=T)

##그룹 만들기
groupa04 <-raw_anova[raw_anova$group=='A',1:2] 
groupb04 <- raw_anova[raw_anova$group=='B',1:2]
groupc04 <-raw_anova[raw_anova$group=='C',1:2]

##평균 차이 나타내기
mean(groupa04[,2])
mean(groupb04[,2])
mean(groupc04[,2])

###ANOVA테스트 과정
##가설설정 - 정규성 검정 - 분산 동질성 검정 - ANOVA 테스트 - 결론

##가설설정
#귀무가설: 세 집단간 평균 차이가 없다.
#대립가설: 세 집단간 평균 차이가 있다.

##정규성 검정
shapiro.test(groupa04[,2])
qqnorm(groupa04[,2])
qqline(groupa04[,2])
shapiro.test(groupb04[,2])
qqnorm(groupb04[,2])
qqline(groupb04[,2])
shapiro.test(groupc04[,2])
qqnorm(groupc04[,2])
qqline(groupc04[,2])
#3집단 모두 귀무가설 채택:데이터가 정규분포를 따른다.

##분산 동질성 검정
#levene테스트
#install.packages('lawstat')
library(lawstat)
levene.test(raw_anova$height,raw_anova$group)      #> 3집단의 분산이 동일하다.
#bartlett테스트
bartlett.test(height ~ group, data=raw_anova)      #> 3집단의 분산이 동일하다.

##ANOVA테스트
RAWANOVA <- aov(height~group,data=raw_anova)
summary(RAWANOVA)                                  #0.05보다 작아서 대립가설 채택! -> 3집단의 평균은 다름



####분할표를 이용한 연관성 분석
###카이제곱검정
##가설 설정
#귀무가설: 흡연여부와 폐암유무는 연관성이 없다.
#대립가설: 흡연여부와 폐암유무는 연관성이 있다.

##데이터 불러오기
raw_chisq <- read.csv('htest01.csv',header=T)
rawTable <- table(raw_chisq)
rawTable

##카이제곱 검정
chisq.test(rawTable,correct=F)                    #셀 기대도수 > 5인 경우: correct=FALSE
                                                  #셀 기대도수 < 5인 경우: correct=TRUE