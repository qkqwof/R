# Week_1 Quiz
# Date : 2021-01-07
# 문제 바로 아래에 답안을 작성하여, r script 형태로 저장 후 제출해주시면 됩니다.

library(dplyr)
library(ggplot2)

# 1. head함수를 활용하여 "mpg" 데이터의 상위 10개 자료를 출력해주세요.
head(mpg,10)


# 2. seq함수를 활용하여 1~50까지 숫자 중 5의 배수를 출력해주세요.
seq(5,50,5)


# 3. 다음과 같은 데이터가 존재합니다. 
# paste 함수를 활용하여 "2021-05-06"이라는 문자열을 출력해주세요. 
year <- "2021"
month <- "05"
day <- "06"
y_m_d <- c(year,month,day)
paste(y_m_d,collapse = '-')


# 4. 1~50까지의 숫자가 있을 때, 각 숫자에 3을 곱한 결과를 출력해주세요
# (예시 : 3, 6, 9, 12, ..., 150)
c(1:50) * 3


# R에는 기본으로 제공하는 내장 데이터와 각 패키지별로 제공하는 데이터셋이 존재합니다.
# 참고 링크 : https://vincentarelbundock.github.io/Rdatasets/datasets.html
# 
# ggplot2 패키지에서는 mpg 외, "diamonds"라는 데이터도 제공합니다.
# 해당 데이터에 대해 "데이터의 크기(행,열)", "변수들의 속성", "요약 통계량"을 확인할 수 있는 함수를 작성해주세요.


# 5. 데이터의 크기
diamonds <- as.data.frame(ggplot2::diamonds)
head(diamonds)
dim(diamonds)

# 6. 변수들의 속성
str(diamonds)


# 7. 요약통계량
summary(diamonds)

