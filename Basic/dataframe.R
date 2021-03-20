##데이터 프레임 이해하기
history <- c(90,80,60,70)
history
math <- c(50,60,100,20)
math
df <- data.frame(history,math)
df
class <- c(1,1,2,2)
class
df1 <- data.frame(history, math, class)
df1
mean(df1$history)
mean(df$math)

##외부데이터 불러오기
##엑셀 파일 불러오기
#readxl 패키지 설치
install.packages('readxl')
library(readxl)
df_finalexam <- read_excel('finalexam.xlsx',sheet=1, col_name=T)
df_finalexam
mean(df_finalexam$math)
mean(df_finalexam$history)
mean(df_finalexam$english)

##csv파일 불러오기
read.csv('csv_exam.csv',header=T)
##csv로 저장하기
write.csv(df_finalexam, file = 'output_newdata.csv')
