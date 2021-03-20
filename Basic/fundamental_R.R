library(ggplot2)
head(mpg)
mean(mpg$hwy)
max(mpg$hwy)
min(mpg$hwy)
hist(mpg$hwy)   #histogram 그래프 만들어주기

## 변수 만들기
a <- 1
a
b <- 2
b
ab <- 3.5
ab
c <- 3
c

##변수로 연산하기
a+b
a+b+c
4/b
5*b

##연속값 변수 만들기
d <- c(1,2,3,4,5)
d
e <- c(1:5)
e
f <- seq(1,5)
f
g <- seq(1,10,by=2)
g
d+e

##문자 변수 만들기
a2 <- 'a'
a
a2
b2 <- 'text'
b2
c2 <- 'Hello world'
c2

##연속 문자 변수 만들기
d2 <- c('a','b','c')
d2
e2 <- c('Hello','world','is','good')
e2

##함수
a <- c(1,2,3)
a
mean(a)
max(a)
min(a)
b <- c('a','a','b','c')
qplot(b)
##문자 처리 함수
e2
paste(e2,collapse=" ")   #빈칸 구분자로 문자 붙이기
e2_paste <- paste(e2,collapse=" ")
e2_paste
e3_paste <- paste(e2,collapse=",")
e3_paste
