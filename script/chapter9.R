# chapter9
# ###############################################
# 작성자 : 이동준
# 작성일 : 20200222
# ###############################################
rm(list = ls())
# 01 데이터 탐색
str(mtcars)
# install.packages("Hmisc")
library("Hmisc")
describe(mtcars)
summary(mtcars)
# fun mean
summary(mpg ~ cyl + hp, data = mtcars)
# fun var
summary(mpg ~ cyl + hp, data = mtcars, fun = var)
summary(cyl ~ mpg + hp, data = mtcars, method = "reverse")
# method = "cross"는 rhs에 나열한 변수의 조합으로 lhs를 요약할 때 사용한다.
summary(mpg ~ cyl + hp, data = mtcars, method = "cross")

plot(iris)
plot(iris$Sepal.Length)
plot(iris$Species)
plot(Species ~ Sepal.Length , data = iris)
with(iris, {
  plot(Sepal.Length, Sepal.Width, pch = as.numeric(Species))
  legend("topright", legend = levels(iris$Species), pch = 1:3)
})

# install.packages("caret", dependencies = T)
library("caret")
featurePlot(iris[, 1:4], iris$Species, "ellipse")

# 02 w전처리
# 모델링에 알맞은 형태로 데이터를 처리하는 것이 preprocessing
# feature scaling
# scale(center T : 모든 데이터에서 평균을 뺀다
# scale = T : 모든 데이터를 전체 데이터의 표준편차로 나눈다)
cbind(as.data.frame(scale(iris[1:4])), iris$Species)

# pca principal component analysis
# 차원 축소 기법 중 하나다
# 변수들을 주성분이라 부르는 선형적 상관관계가 없는 다른 변수들로 재표현한다.
# 주성분들은 원 데이터의 분산을 최대한 보존하는 방법으로 구한다.
x <- 1:10
y <- x + runif(10, min = -0.5, max = 0.5)
z <- x + y + runif(10, min=-10, max=.10)
(data <- data.frame(x, y, z))
head(data)
pr <- princomp(data)
summary(pr)
pr
pr$scores[, 1:2]

# 원 핫 인코딩
(all <- factor(c(paste0(LETTERS, "0"), paste0(LETTERS, "1"))))
(data <- data.frame(lvl = all, value=rnorm(length(all))))
install.packages("randomForest")
library("randomForest")
m <- randomForest(value ~ lvl , data = data)
(x <- data.frame(lvl = factor(c("A", "B", "A", "A", "C")),
                 value = c(1,3,2,4,5)))
model.matrix(~lvl, data= x)[,-1]

# 결측치의 처리
iris_na <- iris
iris_na[c(10, 20, 25, 40, 32), 3] <- NA
iris_na[c(33, 100, 123), 1] <- NA
iris_na[!complete.cases(iris_na),]
iris_na[is.na(iris_na$Sepal.Length),]

install.packages("DMwR")
library("DMwR")
iris_na[!complete.cases(iris_na),]
which(!complete.cases(iris_na))
centralImputation(iris_na[1:4])[which(!complete.cases(iris_na)),]
knnImputation(iris_na[1:4])[which(!complete.cases(iris_na)),]

# 변수 선택
# 0에 가까운 분산
# 분산이 0에 가까운 변수를 쳐내야 한다.
library("caret")
library("mlbench")
data("Soybean")
model <- nearZeroVar(Soybean, saveMetrics = T)
model$nzv
nearZeroVar(Soybean)
(mySoybean <- Soybean[, -nearZeroVar(Soybean)])
head(mySoybean)

# 상관 계수
# 상관 계수가 높은 변수가 여럿 존재하면 파라미터 수가 불필요하게 증가하여 차원 저주에 빠질 우려가 있다.
library("mlbench")
library("caret")
data("Vehicle")
correl <- findCorrelation(cor(subset(Vehicle, select=-c(Class))))
cor(subset(Vehicle, select=-c(Class)))[correl, correl]

# install.packages("FSelector")
library("FSelector")
library("mlbench")
data("Ozone")
head(Ozone)
(v <- linear.correlation(V4 ~ .,
                         data=subset(Ozone, select=-c(V1, V2, V3))))

# 
library("FSelector")
library("mlbench")
data("Vehicle")
(cs <- chi.squared(Class ~ ., data=Vehicle))
cutoff.k(cs, 3)

# 변수의 중요도 평가
library("mlbench")
library("rpart")
library("caret")
data("BreastCancer")
head(BreastCancer)
m <- rpart(Class ~ ., data=BreastCancer)
# 변수의 중요도 평가
varImp(m)

# 03 모델 평가 방법
# 평가 메트릭
predicted <- as.factor(c(1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1))
actual <- as.factor(c(1, 0, 0, 1, 1, 0, 1, 1 ,0, 1, 1, 1))

xtabs ( ~ predicted + actual)
NROW(actual)
nrow(actual)
sum(predicted == actual)/NROW(actual)

library("caret")
confusionMatrix(data = predicted, reference = actual)

# roc 커브
# roc 커브는 fp rate 대비 tp rate의 변화를 뜻한다
# 이러한 이유로 ROC 커브를 그린 뒤 그 아래의 면적을 
# auc area under the curve 라 하여 모델 간의 비교에 사용한다.
library("ROCR")
set.seed(137)
probs <- runif(100)
labels <- as.factor(ifelse(probs > .5 & runif(100) < .4 , "A", "B"))
library("ROCR")
pred <- prediction(probs, labels)
plot(performance(pred, "tpr", "fpr"))
plot(performance(pred, "acc", "cutoff"))
performance(pred, "auc")

# 교차 검증
# 훈련 데이터과 테스트 데이터를 분리하여 모델을 만드는 방법 중 가장
# 자주 사용하는 기법으로 데이터를 다수의 조각으로 나누너 훈련과 테스트를 반복하는 기법
# 교차 검증은 훈련 데이터와 검증 데이터로 나누어 모델링 및 평가하는 작업을 K회 반복하는 것으로
# 이를 K겹 교차 검증이라 한다.
# 최종 성능은 이들의 산술 평균으로 정할 수 있다.
# 과적합
# 주어진데이터로부터 보장되는 것 이상으로 모델을 만들 때 발생한다

install.packages("cvTools")
library("cvTools")
cvFolds(10, K = 5, type = "random")
# 연속된 데이터를 검증으로 사용
cvFolds(10, K = 5, type = "consecutive")
# 연속된 데이터를 차례로 서로 다른 K개의 검증 데이터로 사용
cvFolds(10, K = 5, type = "interleaved")

set.seed(719)
(cv<- cvFolds(NROW(iris), K = 10, R = 3))

head(cv$which, 20)
head(cv$subsets)

(validation_idx <- cv$subsets[which(cv$which == 1), 1])
train <- iris[-validation_idx,]
validation <- iris[validation_idx, ]

library(foreach)
set.seed(719)
R = 3
K = 10
cv <- cvFolds(NROW(iris), K=K, R=R)
foreach(r=1:R) %do% {
  foreach(k=1:K, .combine = c) %do%{
    validation_idx <- cv$subsets[which(cv$which == k), r]
    train <- iris[-validation_idx,]
    validation <- iris[validation_idx,]
    # 데이터 전처리
    
    # 모델 훈련
    
    # 예측
    
    # 성능 평가
    return(0)
  }
}

# 
library(caret)
(parts <- createDataPartition(iris$Species, p = 0.8))
table(iris[parts$Resample1, "Species"])
table(iris[-parts$Resample1, "Species"])
createFolds(iris$Species, k = 10)
createMultiFolds(iris$Species, k = 10, times = 3)
