install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

head(mpg)
dim(mpg)   # 몇 바이 몇인지 보여줌
str(mpg)   # 데이터의 구조를 보여줌
summary(mpg)   # 요약 통계량을 보여줌
View(mpg)   # 원자들을 직접적으로 보여줌(많이 쓰지는 않음)

#1.회사별 평균 연비 높은순 정렬
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean.hwy=mean(hwy)) %>%
  arrange(desc(mean.hwy))

#2.포드 연비 높은순 정렬
mpg %>%
  filter(manufacturer == "ford") %>%
  group_by(model) %>%
  arrange(desc(hwy))

#3.배기량이 연비에 미치는 영향 회귀분석
lm.mpg <- lm(data=mpg, hwy ~ displ) #회귀분석
summary(lm.mpg)  #결과 출력

#4.배기량과 연비 관계 그래프
qplot(data=mpg, x=displ, y= hwy)
