<<<<<<< HEAD
# Week_4 Quiz
# Date : 2021-01-28
# 문제 바로 아래에 답안을 작성하여, <<r script 형태>>로 저장 후 제출해주시면 됩니다. 
# 답안인 코드만 작성하시면 됩니다. 코드 실행 결과 포함 X
# [R Script Encoding Error] : File > Reopen with Encoding > 'UTF-8' > OK

# 아래 다섯 줄의 코드를 실행 후 문제를 풀어주세요.
# BCW는 수업 > [4주차 과제 외부데이터]에 있습니다. 출처 : https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
# 데이터 설명 : 세포 핵의 특성을 분석하여 유방의 종양이 양성인지 분류(Predict whether the cancer is benign or malignant)
library(caret)
library(tree)
set.seed(2020)
bcw <- read.csv("BCW.csv")  # 경로는 본인의 컴퓨터 환경에 맞게 작성하세요.
bcw <- bcw[, -1]    # id 변수 제거

##################################################################################################
# 1번 문제는 decision tree 모형적합에 관련된 문제로, 코드 작성에 어려움이 있으면 해당 실습파일을 참고하셔서 문제를 풀어보세요 (4문제)

# (5점) 1-(1) bcw의 diagnosis 변수를 factor형 변수로 바꿔주세요.
# hint. as.factor함수 이용
bcw$diagnosis <- as.factor(bcw$diagnosis)

# (5점) 1-(2) train 데이터와 test 데이터를 7:3의 비율로 나누어서 지정해주세요. 
anal <- bcw
set.seed(2021)
datatotal <- sort(sample(nrow(anal),nrow(anal)*.7))
train <- bcw[datatotal,]
test <- bcw[-datatotal,]

# (5점) 1-(3) target 변수는 diagnosis 입니다. tree함수를 이용하여 train데이터를 결정나무에 적합시키세요.
treeRaw <- tree(diagnosis~.,data=train)
plot(treeRaw)
text(treeRaw)

# cv.tree 함수를 이용해 그래프로 나타내보니, size = 7에서 가지치기를 하는 것이 좋은 것으로 나타났습니다. 
# (5점) 1-(4) size = 7로 가지치기 한 결정나무를 적합하세요.
prune_tree <- prune.misclass(treeRaw,best=7)
plot(prune_tree)
text(prune_tree,pretty=0)



##################################################################################################
# 2번 문제는 svm에 관련된 문제로, svm 실습 파일을 참고하셔서 문제를 풀어보세요.(2문제)
# 아래 코드를 실행하여 trainControl 설정을 해주세요. 
ctrl <- trainControl(method="repeatedcv",repeats = 3)

# (5점) 2-(1). trControl로 위에서 작성한 ctrl을 이용하는 선형 svm을 적합하세요. 이 때 target변수는 diagnosis입니다.
# hint. train 함수를 이용. method로 적합방법 지정
svm_linear_fit <- train(diagnosis~.,data=train,
                        method='svmLinear',
                        trControl=ctrl,
                        preProcess=c('center','scale'),
                        metric='Accuracy')
svm_linear_fit

# (10점) 2-(2). 2-(1)에서 적합한 모형으로 test 데이터의 diagnosis값을 예측한 후, 혼동행렬을 만드는 코드를 작성하세요.
pred <- predict(svm_linear_fit,newdata=test)
confusionMatrix(pred,test$diagnosis)

#################################################################################################
# 3번은 pca와 관련된 문제입니다. 해당 실습 파일을 참고하여 문제를 풀어보세요.(2문제)
# (10점) 3-(1) bcw의 첫 번째 열을 제외하고는 모두 수치형 자료입니다. 2번째 열부터 31번째 열까지 pca함수를 적용하세요. 
# 이 때, 변수의 표준화를 반드시 실행하세요. 
summary(bcw)
bcw.pca <- prcomp(bcw[2:31],center=T,scale.=T)    
summary(bcw.pca)      
bcw.pca$rotation      
head(bcw.pca$x,10)  

# (5점) 3-(2) 분산의 90% 이상을 설명하기 위해 몇 개의 주성분 변수를 사용해야 하나요?
# hint. summary함수를 이용하면 pca를 실행한 요약 결과를 확인할 수 있습니다. Cumulative Proportion를 확인하세요.
# 코드를 아래에 작성하세요. (summary(3-(1)결과))
summary(bcw.pca)      
bcw.pca$rotation      
head(bcw.pca$x,10)
plot(bcw.pca, type = 'l', main = 'Scree Plot') 

# 정답 : 7


#################################################################################################
# 4. kmeans (1문제)
# (10점) 4. 2번째 열부터 31번째 열을 이용해서 k-means clustering을 실시하세요. 이 때 k의 수는 5개로 지정하세요.
install.packages("factoextra")
library(factoextra)
set.seed(2021)
fviz_nbclust(bcw[,2:31],kmeans,method='wss',k.max=15) + theme_minimal()+ggtitle('Elbow method')
bcw.kmeans <- kmeans(bcw[,2:31],centers=5,iter.max=1000)
bcw.kmeans



##################################################################################################
# 5번 문제는 hierarchical clustering(계층적 군집분석)에 관련된 문제입니다. 해당 실습 문서를 참고하여 문제를 풀어보세요. (2문제)
# bcw 데이터의 2번째 열부터 31번째 열을 이용하여 계층적 군집분석을 실시하려고 합니다.
# 아래 코드를 실행하여 유클리드 거리를 기반으로 하는 유사도행렬을 생성하세요.
bcw.dist <- dist(bcw[,2:31],  method = 'euclidean')

# (5점) 5-(1) bcw.dist를 이용해 ward's method로 계층적 군집분석을 실시하세요.
bcw.hclust.ward <- hclust(bcw.dist,method='ward.D2')
bcw.hclust.ward
plot(bcw.hclust.ward, cex = 0.6, hang = -1)
rect.hclust(bcw.hclust.ward, k = 5, border = 2:5)


# 5-(1)에서 만든 군집 분석 결과물을 확인하였더니 5개의 군집으로 구성하는 것이 적당해보입니다.
# (5점) 5-(2) 원래의 데이터 bcw에 군집분석한 결과물을 이용해 cluster라는 변수를 생성해주고 싶습니다.
# cutree함수를 이용해 bcw에 cluster라는 변수를 생성해주세요(군집 5개, 5-(1)결과물 이용)
bcw.clusters <- cutree(bcw.hclust.ward, k =5)
table(bcw.clusters)
bcw$cluster <- bcw.clusters
head(bcw)

fviz_cluster(list(data = bcw[,2:31], cluster = bcw.clusters))
=======
# Week_4 Quiz
# Date : 2021-01-28
# 문제 바로 아래에 답안을 작성하여, <<r script 형태>>로 저장 후 제출해주시면 됩니다. 
# 답안인 코드만 작성하시면 됩니다. 코드 실행 결과 포함 X
# [R Script Encoding Error] : File > Reopen with Encoding > 'UTF-8' > OK

# 아래 다섯 줄의 코드를 실행 후 문제를 풀어주세요.
# BCW는 수업 > [4주차 과제 외부데이터]에 있습니다. 출처 : https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
# 데이터 설명 : 세포 핵의 특성을 분석하여 유방의 종양이 양성인지 분류(Predict whether the cancer is benign or malignant)
library(caret)
library(tree)
set.seed(2020)
bcw <- read.csv("BCW.csv")  # 경로는 본인의 컴퓨터 환경에 맞게 작성하세요.
bcw <- bcw[, -1]    # id 변수 제거

##################################################################################################
# 1번 문제는 decision tree 모형적합에 관련된 문제로, 코드 작성에 어려움이 있으면 해당 실습파일을 참고하셔서 문제를 풀어보세요 (4문제)

# (5점) 1-(1) bcw의 diagnosis 변수를 factor형 변수로 바꿔주세요.
# hint. as.factor함수 이용
bcw$diagnosis <- as.factor(bcw$diagnosis)

# (5점) 1-(2) train 데이터와 test 데이터를 7:3의 비율로 나누어서 지정해주세요. 
anal <- bcw
set.seed(2021)
datatotal <- sort(sample(nrow(anal),nrow(anal)*.7))
train <- bcw[datatotal,]
test <- bcw[-datatotal,]

# (5점) 1-(3) target 변수는 diagnosis 입니다. tree함수를 이용하여 train데이터를 결정나무에 적합시키세요.
treeRaw <- tree(diagnosis~.,data=train)
plot(treeRaw)
text(treeRaw)

# cv.tree 함수를 이용해 그래프로 나타내보니, size = 7에서 가지치기를 하는 것이 좋은 것으로 나타났습니다. 
# (5점) 1-(4) size = 7로 가지치기 한 결정나무를 적합하세요.
prune_tree <- prune.misclass(treeRaw,best=7)
plot(prune_tree)
text(prune_tree,pretty=0)



##################################################################################################
# 2번 문제는 svm에 관련된 문제로, svm 실습 파일을 참고하셔서 문제를 풀어보세요.(2문제)
# 아래 코드를 실행하여 trainControl 설정을 해주세요. 
ctrl <- trainControl(method="repeatedcv",repeats = 3)

# (5점) 2-(1). trControl로 위에서 작성한 ctrl을 이용하는 선형 svm을 적합하세요. 이 때 target변수는 diagnosis입니다.
# hint. train 함수를 이용. method로 적합방법 지정
svm_linear_fit <- train(diagnosis~.,data=train,
                        method='svmLinear',
                        trControl=ctrl,
                        preProcess=c('center','scale'),
                        metric='Accuracy')
svm_linear_fit

# (10점) 2-(2). 2-(1)에서 적합한 모형으로 test 데이터의 diagnosis값을 예측한 후, 혼동행렬을 만드는 코드를 작성하세요.
pred <- predict(svm_linear_fit,newdata=test)
confusionMatrix(pred,test$diagnosis)

#################################################################################################
# 3번은 pca와 관련된 문제입니다. 해당 실습 파일을 참고하여 문제를 풀어보세요.(2문제)
# (10점) 3-(1) bcw의 첫 번째 열을 제외하고는 모두 수치형 자료입니다. 2번째 열부터 31번째 열까지 pca함수를 적용하세요. 
# 이 때, 변수의 표준화를 반드시 실행하세요. 
summary(bcw)
bcw.pca <- prcomp(bcw[2:31],center=T,scale.=T)    
summary(bcw.pca)      
bcw.pca$rotation      
head(bcw.pca$x,10)  

# (5점) 3-(2) 분산의 90% 이상을 설명하기 위해 몇 개의 주성분 변수를 사용해야 하나요?
# hint. summary함수를 이용하면 pca를 실행한 요약 결과를 확인할 수 있습니다. Cumulative Proportion를 확인하세요.
# 코드를 아래에 작성하세요. (summary(3-(1)결과))
summary(bcw.pca)      
bcw.pca$rotation      
head(bcw.pca$x,10)
plot(bcw.pca, type = 'l', main = 'Scree Plot') 

# 정답 : 7


#################################################################################################
# 4. kmeans (1문제)
# (10점) 4. 2번째 열부터 31번째 열을 이용해서 k-means clustering을 실시하세요. 이 때 k의 수는 5개로 지정하세요.
install.packages("factoextra")
library(factoextra)
set.seed(2021)
fviz_nbclust(bcw[,2:31],kmeans,method='wss',k.max=15) + theme_minimal()+ggtitle('Elbow method')
bcw.kmeans <- kmeans(bcw[,2:31],centers=5,iter.max=1000)
bcw.kmeans



##################################################################################################
# 5번 문제는 hierarchical clustering(계층적 군집분석)에 관련된 문제입니다. 해당 실습 문서를 참고하여 문제를 풀어보세요. (2문제)
# bcw 데이터의 2번째 열부터 31번째 열을 이용하여 계층적 군집분석을 실시하려고 합니다.
# 아래 코드를 실행하여 유클리드 거리를 기반으로 하는 유사도행렬을 생성하세요.
bcw.dist <- dist(bcw[,2:31],  method = 'euclidean')

# (5점) 5-(1) bcw.dist를 이용해 ward's method로 계층적 군집분석을 실시하세요.
bcw.hclust.ward <- hclust(bcw.dist,method='ward.D2')
bcw.hclust.ward
plot(bcw.hclust.ward, cex = 0.6, hang = -1)
rect.hclust(bcw.hclust.ward, k = 5, border = 2:5)


# 5-(1)에서 만든 군집 분석 결과물을 확인하였더니 5개의 군집으로 구성하는 것이 적당해보입니다.
# (5점) 5-(2) 원래의 데이터 bcw에 군집분석한 결과물을 이용해 cluster라는 변수를 생성해주고 싶습니다.
# cutree함수를 이용해 bcw에 cluster라는 변수를 생성해주세요(군집 5개, 5-(1)결과물 이용)
bcw.clusters <- cutree(bcw.hclust.ward, k =5)
table(bcw.clusters)
bcw$cluster <- bcw.clusters
head(bcw)

fviz_cluster(list(data = bcw[,2:31], cluster = bcw.clusters))
>>>>>>> c3a0582dd9af788e0400bb6bdf7b8dbae6a9ad9d
