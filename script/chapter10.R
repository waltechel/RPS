# chapter10
# ###############################################
# 작성자 : 이동준
# 작성일 : 20200229

# ###############################################
rm(list = ls())

# 01 로지스틱 회귀 모델
d <- subset(iris, iris$Species == "virginica" | iris$Species == "versicolor")
str(d)

d$Species <- factor(d$Species)
str(d)

(m <- glm(data = d, formula = Species ~ ., family = "binomial"))
fitted(m)[c(1:5, 51:55)]

f <- fitted(m)
as.numeric(d$Species)
length(d$Species)
sum(ifelse(f > 0.5 , 1, 0) == as.numeric(d$Species) - 1)
sum(ifelse(f > 0.5 , 1, 0) == as.numeric(d$Species) - 1) / length(d$Species)

predict(m, newdata = d[c(1,10,55),], type = "response")

# 02 다항 로지스틱 회귀 분석
library(nnet)
# 다항 로지스틱 회귀 모델을 사용한 예측을 수행한다.
(m <- multinom(Species~., data = iris))
head(fitted(m))
predict(m, newdata = iris[c(1, 51, 101), ], type = "class")
predict(m, newdata = iris[c(1, 51, 101), ], type = "probs")

predicted <- predict(m, iris)
sum(predicted == iris$Species) / NROW(predicted)
xtabs(~ predicted + iris$Species)


# 03 의사 결정 나무
# rpart, ctree, randomForest
# 의사결정 나무의 노드를 나누는 기준
# : 가장 좋은 질문은 한 노드의 데이터를 두 개의 자식 노드로 분리했을 때 자식 노드들의 불순도가 가장 낮아지는 질문이다.

# install.packages("rpart")
library("rpart")
(m <- rpart(Species ~ . , data = iris))
plot(m, compress = T, margin = 0.2)
text(m, cex = 1.5)

# install.packages("rpart.plot")
library("rpart.plot")
prp(m, type = 4, extra = 2, digits = 3)

head(predict(m, newdata = iris, type = "class"))

# 조건부 추론 나무
library("party")
(m <- ctree(Species ~ ., data=iris))
plot(m)

# 랜덤 포레스트
# 앙상블 학습은 주어진 데이터로부터 여러 개의 모델을 학습한 다음, 예측 시 여러 모델의 예측
# 결과들을 종합해 사용하여 정확도를 높이는 기법을 말한다.
# 새로운 데이터에 대한 예측을 수행할 때는 여러 개의 의사 결정 나무가 내놓은 예측 결과를 투표 방식으로 합한다.
# 랜덤 포레스트는 일반적으로 성능이 뛰어나고 의사 결정 나무가 아니라 여러 개를 사용해서 과적합 문제를 피한다.
library("randomForest")
m <- randomForest(Species ~ ., data =iris)
m
# OOB : out of bag estimate of error rate : 4% 는 모델 훈련에 사용되지 않은 데이터를 
# 사용한 에러 추정치를 말한다.
head(predict(m, newdata = iris))
m <- randomForest(iris[,1:4], iris[,5])
m

m <- randomForest(Species ~ . , data= iris, importance = T)
# MeanDecreaseAccuracy 가 높을 수록 좋은 항목이다.
importance(m)
varImpPlot(m, main = "varImpPlot of iris")

# 파라미터 튜닝
# randomForest()에는 나무 개수, 각 노드를 자식 노드로 나누는 기준을 정할 때 고려할 변수의 개수mtry 파라미터가 있다.
# 이 파라미터를 튜닝하면 성능이 개선되는데, 이들 파라미터를 정하는 한 가지 방법이 교차 검증을 사용하는 것이다.
(grid <- expand.grid(ntree=c(10, 100, 200), mtry = c(3,4)))
library(cvTools)
library(foreach)
library(randomForest)
set.seed(719)
K = 10
R = 3
cv <- cvFolds(NROW(iris), K=K, R=R)
grid <- expand.grid(ntree=c(10, 100, 200), mtry = c(3, 4))

result <- foreach(g=1:NROW(grid), .combine = rbind) %do% {
  foreach(r= 1:R, .combine = rbind) %do% {
    foreach(k=1:K, .combine = rbind) %do%  {
      validation_idx <- cv$subsets[which(cv$which==k), r]
      train <- iris[-validation_idx,]
      validation <- iris[validation_idx,]
      # 모델 훈련
      m <- randomForest(Species ~.,
                        data= train,
                        ntree=grid[g,"ntree"],
                        mtry = grid[g,"mtry"]
                        )
      predicted <- predict(m, newdata = validation)
      precision <- sum(predicted==validation$Species) / NROW(predicted)
      return (data.frame(g = g, precision = precision))
    }
  }
}
result

library("plyr")
ddply(result, .(g), summarize, mean_precision = mean(precision))
result
grid[c(4,6),]

# 04 신경망
# 입력층 에 주어진 값들이 은닉층 노드로 모일 때 은닉층 노드로 전달되는 이 값을 
# 넷 활성화net activation이라 하고,이와 같은 계산식을 합성 함수라고 한다.
# 은닉층 노드가 출력층에 보내는 값은 넷 활성화 값을 활성 함수에 통과시킨 결과이다.
# 이 때 주로 사용하는 함수가 sigmoid시그모이드 함수이다.

# 신경망 학습은 가중치 w(입력층->은닉층에 부여되는 가중치)의 조절 작업이다
# 모델의 출력 값이 원하는 출력값과 얼마나 같은지를 비교하는 기준에는 에러의 제곱합과
# 정보이론의 엔트로피가 있다.
# 차이가 존재하면 거슬러 올라가면서 가중치를 조절하여 가중치가 0에 가까워지도록 만드는데
# 이를 역전파 back propagation algorithm 이라고 한다.
# 과적합을 피하는 방법은 역전파 알고리즘에 따라 가중치를 교정할 때마다
# 0과 1 사이의 값을 가지는 ε을 가중치에 곱해주는 것이다. 이를 가중치 감소라고 한다.
# 신경망의 파라미터는 엔트로피 또는 오차 제곱합을 고려해 최적화된다.
# 과적합을 막기 위한 방법으로 가중치 감소를 R에서 제공하고 있다.

library(nnet)
m <- nnet(Species ~ . , data=iris, size = 3)
(prediction<-predict(m, newdata = iris, type = "class"))
sum(prediction == iris$Species)/ NROW(iris)
class.ind(iris$Species)
m2 <- nnet(iris[,1:4], class.ind(iris$Species), size=3, softmax = T)
predict(m2, newdata = iris[,1:4],type = "class")

# 05 서포트 벡터 머신
# SVM은 서로 다른 분류에 속한 데이터 간에 간격이 최대가 되는 선 또는 평면을 찾아 이를 기준으로 데티어를 분류한다.
# 서포트 벡터 머신은 각 분류에 속하는 데이터로부터 같은 간격으로 그리고 최대로 멀리 떨어진 선 또는 평면을 찾는다. 이러한 선 또는 평면을 최대 여백 초평면이라고 하고 이를 분류를 나누는 기준으로 사용한다.
# 서포트 벡터 머신 모델의 핵심 수식은 벡터 간의 내적 계산이다.
# 커널 트릭에서는 실제로 데이터를 고차원으로 변환하는 대신 고차원에서 벡터 간 내적 계산을 했을 때와 같은 값을 반환하는 함수들을 사용한다. 이 함수들을 사용하면 마치 데이터를 고차원으로 옮긴 듯한 효과를 일으키면서도 데이터를 고차원으로 옮기는 데 따른 계산 비용 증가는 피할 수 있다.

# install.packages("kernlab")
library("kernlab")
(m <- ksvm(Species ~ ., data = iris))
head(predict(m, newdata = iris))
# vanilladot(특별한 변환 없이 내적을 계산한다는 뜻)
ksvm(Species ~ ., data = iris, kernal="vanilladot")
# 커널에 사용하는 파라미터는 kpar에 리스트 형태로 지정한다
# 다음은 3차 다항 커널을 사용한다.
(m <- ksvm(Species ~., data=iris, kernel="polydot",kpar=list(degree=3)))

# 이거 어떻게 하는 것인지 모르겠다.
library("e1071")
tune(svm, Species ~ .,data=iris, gamma=2^(-1:1),cost=2^(2:4))
result<-tune(svm, Species ~ .,data=iris, gamma=2^(-1:1),cost=2^(2:4))
result
attributes(result)
result$best.parameters
result$best.parameters["gamma"]
result$best.parameters["cost"]

# 06 클래스 불균형
library("mlbench")
data("BreastCancer")
table(BreastCancer$Class)
# 클래스 불균형을 해결하는 한가지 방법은 관찰 데이터가 적은 쪽의 데이터에 더 큰 가중치를 주거나
# 데이터가 적은 쪽을 잘못 분류했을 때 더 많은 비용을 부과하는 것이다.
# 또 다른 방법은 모델을 만드는 훈련 데이터를 직접 조절하는 방법이다. 이에는 
# 업샘플링, 다운 샘플링, SMOTE synthetic Minority Oversampling Technique
# 업샘플링은 해당 분류에 속하는 데이터가 적은 쪽을 표본으로 더 많이 추출하는 방법이뎌
# 다운 샘플링은 데이터가 많은 쪽을 적게 추출하는 방법이다.
library("caret")
head(BreastCancer)
x <- upSample(subset(BreastCancer, select=-Class), BreastCancer$Class)
table(BreastCancer$Class)
table(x$Class)
NROW(x)
NROW(unique(x))

llibrary(party)
data <- subset(BreastCancer, select=-Id)
head(data)
parts <- createDataPartition(data$Class, p=0.8)
head(parts)
data.train <- data[parts$Resample1,]
data.test <- data[-parts$Resample1,]
m <- rpart(Class ~., data=data.train)
result1 <- confusionMatrix(data.test$Class,
                predict(m, newdata = data.test, type = "class"))
result1$overall[1]

data.up.train <- upSample(subset(data.train, select=-Class),
                          data.train$Class)
m <- rpart(Class ~ ., data=data.up.train)
result2 <- confusionMatrix(data.test$Class,
                predict(m, data.test, type="class"))
result2$overall[1]

# SMOTE
# 비율이 낮은 분류의 데이터를 만들어내는 방법이다.
# SMOTE는 먼저 분류 개수가 적은 쪽의 데이터 샘플을 취한 뒤 이 샘플의 K 최근접 이웃을 찾는다.
# 결과적으로 SMOTE는 기존의 샘플을 주변의 이웃을 고려해 약간씩 이동시킨 점들을 추가하는 방식으로 동작한다.

data(iris)
data <- iris[, c(1,2,5)]
head(data)
data$Species<-factor(ifelse(data$Species == "setosa", "rare", "common"))
sum(data$Species=="rare")
sum(!data$Species=="rare")
library("DMwR")
# perc.over = 600 적은 쪽의 데이터를 얼마나 추가로 샘플링해야 하는지
# perc.under = 200 많은 쪽의 데이터를 얼마나 샘플링할지
newdata <- SMOTE(Species ~ ., data, prec.over = 600, prec.under = 100)
nrow(newdata)
sum(newdata$Species=="rare")
sum(!newdata$Species=="rare")

# 07 문서 분류
# Document Classification은 주어진 문서를 하나 이상의 분류로 구분하는 문제이다.
# 제품 리뷰 글을 보고 해당 리뷰가 제품에 대한 긍정적인 리뷰인지 부정적인 리뷰인지를 구분하는 감성 분석이다.
# install.packages("tm")
library(tm)
data(crude)
summary(crude)
inspect(crude[1])

# 문서 전처리
inspect(tm_map(tm_map(crude, tolower), removePunctuation)[1])

# 문서의 행렬 표현
# 문서를 기술하는 표현을 문서로부터 추출하고 이로부터 분류를 예측하는 알고리즘을 만들어야 한다.
# 문서의 행렬 표현 방식은 이러한 목적으로 가장 많이 사용되는 기법이다.
# 단어의 출현 빈도임을 알 수 있다.
(x <- TermDocumentMatrix(crude))
inspect(x[1:10, 1:10])
x <- TermDocumentMatrix(crude, control=list(weighting=weightTfIdf))
inspect(x[1:10, 1:5])
findFreqTerms(TermDocumentMatrix(crude), lowfreq = 10)

# 단어 간 상관관계
findAssocs(TermDocumentMatrix(crude), "oil", 0.7)

# 문서 분류
data("crude")
data(acq)
to_dtm <- function(corpus){
  x <- tm_map(corpus, tolower)
  x <- tm_map(corpus, removePunctuation)
  return(DocumentTermMatrix(x))
}
crude_acq <- c(to_dtm(crude), to_dtm(acq))
crude_acq
crude_acq_df <- cbind(
  as.data.frame(as.matrix(crude_acq)),
  LABEL=c(rep("crude", 20), rep("acq", 50))
)

# 모델링
library("caret")
train_idx <- createDataPartition(crude_acq_df$LABEL, p=0.8)$Resample1
crude_acq.train <- crude_acq_df[train_idx,]
crude_acq.test <- crude_acq_df[-train_idx,]

library("rpart")
m <- rpart(LABEL ~ ., data=crude_acq.train)
confusionMatrix(
  predict(m, newdata=crude_acq.test, type = "class"),
  crude_acq.test$LABEL
)

(docs <- read.csv("script/docs.csv", stringsAsFactors = F))
str(docs)
library("tm")
colnames(docs) <- c("doc_id", "text", "Label")
corpus <- Corpus(DataframeSource(docs[,1:2]))
head(docs)

# 메타 데이터
data(crude)
meta(crude, type="corpus")
meta(crude, type="local")
meta(crude[1], type="local")

# 08
# caret 패키지
library("caret")
set.seed(137)
train.idx <- createDataPartition(iris$Species, p=0.8)[[1]]
data.train <- iris[train.idx,]
data.test <- iris[-train.idx,]
(m <- train(Species~.,
            data=data.train,
            preProcess = c("pca"),
            method="rf",
            ntree=1000,
            trControl=trainControl(method = "cv", number=10, repeats = 3)))

confusionMatrix(
  predict(m, newdata=data.test,type="raw"),
  data.test$Species
)
