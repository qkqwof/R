####그래프 만들기
###변수 간 관계를 표현하는 산점도
#ggplot2로드
library(ggplot2)
##1.배경 설정하기
#x축 displ, y축 hwy로 지정해 배경 생성
ggplot(data=mpg,aes(x=displ, y=hwy))
##2.그래프 추가하기
#배경에 산점도 추가
ggplot(data=mpg,aes(x=displ,y=hwy)) + geom_point()
##3.축 범위를 조정하는 설정 추가하기
#x축 범위 3~6으로 지정, y축 범위 10~30으로 지정
ggplot(data=mpg,aes(x=displ,y=hwy)) + geom_point() + xlim(3,6) + ylim(10,30)

##혼자서 해보기##
#Q1. mpg데이터의 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 
#    알아보려고 합니다. x축은 cty, y축은 hwy로 된 산점도를 만들어 보세요.
library(ggplot2)
ggplot(data=mpg,aes(x=cty,y=hwy)) + geom_point()

#Q2. 미국 지역별 인구통계 정보를 담은 ggplot패키지의 midwest데이터를 이용해서 
#    전체 인구와 아시아인 인구 간에 어떤 관계가 있는지 알아보려고 합니다.
#    x축은 poptotal(전체 인구),y축은 popasian(아시아인 인구)으로 된 산점도를
#    만들어 보세요. 전체 인구는 50만 명 이하, 아시아인 인구는 1만 명 이하인 지역
#    만 산점도에 표시되게 설정하세요.
midwest <- as.data.frame(ggplot2::midwest)
ggplot(data=midwest, aes(x = poptotal, y = popasian)) + geom_point() + xlim(0,500000) + ylim(0,10000)

