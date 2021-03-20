###이상치 정제하기 - 정상범주에서 크게 벗어난 값
##이상치 제거하기 - 존재할 수 없는 값(sex:3,score:6)
outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))
#이상치 확인
table(outlier$sex)
table(outlier$score)
#결측 처리하기-sex
#sex가 3이면 NA할당
outlier$sex <- ifelse(outlier$sex==3,NA,outlier$sex)
outlier
#score가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score >5,NA,outlier$score)
outlier
##결측치 제외하고 분석
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score))

###이상치 제거하기 - 극단적인 값
#정상 범위 기준 정해서 벗어나면 결측 처리
#논리적 판단: 성인 몸무게 40~150kg 벗어나면 극단치
#통계적 판단: 상하위 0.3% 극단치 또는 상자그림 1.5IQR 벗어나면 극단치

#상자그림으로 극단치 기준 정해서 제거하기
#상자그램 생성
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)
#상자그림 통계치 출력
boxplot(mpg$hwy)$stats     #상자그림 통계치 출력
#결측치 처리 
#12~37 벗어나면 NA 할당
mpg$hwy <- ifelse(mpg$hwy>12|mpg$hwy <37,NA,mpg$hwy)
table(is.na(mpg$hwy))
#결측치 제외하고 분석하기
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))

###혼자서 해보기
