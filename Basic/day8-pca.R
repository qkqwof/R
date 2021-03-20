####데이터 파악
### 1. 데이터 확인 
head(iris)

### 2. 결측치 확인
colSums(is.na(iris))

### 3. 변수별 기술통계 및 분포 확인
summary(iris)
boxplot(iris[,1:4])

### 4. pca 함수 적용 및 요약 결과 확인
iris.pca <- prcomp(iris[1:4],center=T,scale.=T)    #pca함수 -> 평균은 0, 분산은 1
summary(iris.pca)      #pca 요약정보. standard deviation 제곱 = 분산 = eivenvalue
iris.pca$rotation      #각 주성분의 eigenvector
head(iris.pca$x,10)    #각 주성분의 값

### 5. scree plot 확인
plot(iris.pca,type='l',main = 'Scree Plot')        #pc의 분산을 y축으로 scree plot 생성      #l -> line으로 그려달라

### 6. 차원축소
head(iris.pca$x[,1:2],10)       #2개의 차원으로 축소(scree plot에서 2개의 값이 적당했으니까!!)

### 7. 2차원 시각화
?autoplot
library('plotfortify')
autoplot(iris.pca,data = iris,colour = 'Species') 

####실습 2
###1. 데이터 확인
install.packages('jpeg')
library(jpeg)
cat <- readJPEG('cat.jpeg')
class(cat)
dim(cat)

###2. rgb 데이터 분할 및 주성분 분석
r <- cat[,,1]                        #array에서 r에 해당하는 데이터
g <- cat[,,2]                        #array에서 g에 해당하는 데이터
b <- cat[,,3]                        #array에서 b에 해당하는 데이터
cat.r.pca <- prcomp(r,center=F)      #r 데이터 주성분분석     -> center=F => 표준화를 안하고 돌림(차원 축소 후 사진을 다시 만들어 줘야하기 때문!!!) 
cat.g.pca <- prcomp(g,center=F)      #g 데이터 주성분분석
cat.b.pca <- prcomp(b,center=F)      #b 데이터 주성분분석
rgb.pca <- list(cat.r.pca,cat.g.pca,cat.b.pca)            #분석 결과 rgb로 합침

###3. 차원축소하여 jpg로 저장
pc <- c(2,10,50,100,300)    #축소할 차원 수
for (i in pc) {
  pca.img <- sapply(rgb.pca, function (j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  },simplify = 'array')
  writeJPEG(pca.img,paste('cat_pca_',i,'.jpeg',sep=''))
}
