#파일 불러오기
rawN3 <- read.csv('htest01.csv',header=T)
rawN3

#그룹 나누기
groupA <- rawN3[rawN3$group=='A',1:2]
groupB <- rawN3[rawN3$group=='B',1:2]

#각 집단의 평균 구해서 비교하기
mean(groupA[,2])
mean(groupB[,2])

##T-test 과정
# 가설검정 - 데이터 정규성 검정 - 분산 동질성 검정 - T-test - 결론

##가설 설정
#귀무가설: 그룹A,구룹B 간 평균 키 차이 없다.
#대립가설: 그룹B의 평균 키가 그룹A의 평균 키보다 크다.

##정규성 검정
shapiro.test(groupA[,2])

#플롯 그리기
qqnorm(groupA[,2])
qqline(groupA[,2])

shapiro.test(groupB[,2])

qqnorm(groupB[,2])
qqline(groupB[,2])

##분산 동질성 검정
var.test(groupA[,2],groupB[,2])          #분산이 동일하다.

##T-test
t.test(groupA[,2],groupB[,2],alternative = 'less',var.equal = T)   #alternative = 'less': 대립가설에서 왼쪽값이 오른쪽보다 작다는 뜻!!!

##결론
#p-value가 0.05보다 작기 때문에 귀무가설을 채택하고 두 집단 평균은 동일하다.


###샘플 사이즈가 10개인 경우
##데이터 불러오기
rawN10 <- read.csv('htest02.csv',header=T)
rawN10

##집단 나누기
groupa <- rawN10[rawN10$group=='A',1:2]
groupb <- rawN10[rawN10$group=='B',1:2]

##각 집단의 평균 구해서 비교하기
mean(groupa[,2])
mean(groupb[,2])

###가설 검정
#귀무가설: 그룹A,그룹B 간 평균 키 차이 없다.
#대립가설: 그룹B의 평균 키가 그룹A의 평균 키보다 크다.

###정규성 검정
shapiro.test(groupa[,2])
qqnorm(groupa[,2])
qqline(groupa[,2])

shapiro.test(groupb[,2])
qqnorm(groupb[,2])
qqline(groupb[,2])

###분산 동질성 검정
var.test(groupa[,2],groupb[,2])

###T-test
t.test(groupa[,2],groupb[,2],alternative = 'less',var.equal = F)

###결론
#p-value가 0.05보다 작아서 대립가설 채택!! -> 그룹B의 평균 키가 그룹A의 평균 키보다 크다.


####대응표본 t검정
###데이터 불러오기
raw_d <- read.csv('htest02d.csv',header = T)
raw_d
groupAd <- raw_d[,1]
groupBd <- raw_d[,2]

###각 집단의 평균 구해서 비교하기
mean(groupAd)
mean(groupBd)

##가설 검정
#귀무가설: 마케팅을 통해 판매액 변화 없음
#대립가설: 마케팅을 통해 판매액이 증가

##정규성 검정
d <- groupAd - groupBd     #두 집단의 차이를 검정하는 것!!
shapiro.test(d)            # 0.05보다 커서 귀무 가설 채택 -> 정규성이 있다.
qqnorm(d)
qqline(d)

#대응표본 T검정
t.test(groupAd, groupBd,alternative = 'less',paired=T)       #piared = T:한 쌍을 이루는가를 물어보는 것 -> 대응표본이다.

##결론
#0.05보다 작아서 대립가설 채택!!

####샘플 사이즈가 30개 이상일 때 -> 대표본이면 정규분포를 따름
###Z검정

##데이터 불러오기
rawN30 <- read.csv('htest03.csv',header=T)
rawN30

#그룹 나누기
groupA03 <- rawN30[rawN30$group=='A',1:2]
groupB03 <- rawN30[rawN30$group=='B',1:2]

#집단별 평균 구하기
mean(groupA03[,2])
mean(groupB03[,2])

##가설설정
#귀무가설:그룹A,그룹B간 평균 키 차이 없다.
#대립가설:그룹B의 평균 키가 그룹A의 평균 키보다 크다.

##Z-test
z.test(groupA3[,2],groupB3[,2])

#결론
#0.05보다 작아서 대립 가설을 채택!! -> 그룹B의 평균 키가 그룹A의 평균 키보다 크다.

###z테스트를 써야 하는 상황에서 t테스트를 쓰게 되면 분석 결과가 달라짐
