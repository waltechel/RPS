# chapter8
# ###############################################
# 작성자 : 이동준
# 작성일 : 20200222
# ###############################################

# 01 선형 회귀의 기본 가정
# 종속 변수와 독립 변수 간에 선형성이 성립한다
# 독립변수는 고정된 값이다.
# 오차는 정규분포를 따른다.
# 독립 변수 간에는 다중 공선성이 적어야 한다. 

# 02 단순 선형 회귀
# 종속 변수 y1을 하나의 독립변수 x1으로 설명한다.
# 두 개 이상의 독립변수로 설명하는 경우는 중선형 회귀라한다.

# 모델 생성
data(cars)
head(cars)
(m <- lm(dist ~ speed , cars))
# dist = -17.359 + 3.932 * speed + ε

# 선형 회귀 결과 추출
# 회귀 계수 : coef(model)
coef(m)
# 적합값 : car 데이터의 각 speed 값에 대한 모델에 의해 예측된 값은 fitted()로 구할 수 있다.
fitted(m)[1:4]
# 잔차 : residuals(model)
residuals(m)[1:4]
# 적합된 값과 잔차의 합은 실제 데이터 값과 같다
fitted(m)[1:4] + residuals(m)[1:4]
cars$dist[1:4]
# 회귀 계수의 신뢰 구간(confint(model))
confint(m)
# 잔차 제곱합
deviance(m)

# 예측과 신뢰 구간
(m <- lm(dist ~ speed, data = cars))
predict(m, newdata = data.frame(speed=3))
answer <- coef(m)
answer[1] + answer[2] * 3

# 회귀 계수의 신뢰 구간을 구하기 위해 type = "confidence"를 지정하면
# 제동거리의 평균 신뢰 구간을 구할 수 있다.
predict(m, newdata = data.frame(speed=c(3)), interval = "confidence")
# 오차를 포함한 예측 구간을 구할 수도 있다.
predict(m, newdata = data.frame(speed=c(3)), interval = "prediction")

# 모델 평가
summary(m)
# spped 계수 3.9324rk 0이 아니라고 결론을 내릴 수 있다.
# F 통계량은
# MSR/MSE의 비율을 F 분포를 사용해 검정한 것이며, 
# 이 값은 dist = β0 + ε 의 축소 모델과 dist = β0 + β1 * speed + ε의 완전 모델 간에
# 잔차제곱합이 얼마나 유의하게 다른지 보는 방식과 같다.

# 분산 분석 및 모델 간의 비교
anova(m)

# 원래 사용 모델
(full <- lm(dist ~ speed , data = cars))
# dist를 상숫값으로 예측한 경우
(reduced <- lm(dist ~ 1 , data = cars))

# 다음은 anova()로 full과 reduced를 비교한 결과다
anova(reduced, full)
# reduced 모델과 full 모델 간에는 유의한 차이가 있다고 결론을 내린다.
# speed 열이 유의미한 설명 변수임을 뜻한다.

plot(m)
# plot1 기울기 0을 보이는 것이 이상적이다.
# 선형 회귀에서 오차는 평균이 0이고 분산이 일정한 정규분포를 가정
# plot2 잔차 정규분포 확인
# plot3 0에서 멀리 떨어진 값은 이상치일 가능성이 있다.
# plot4 쿡의 거리 확인(회귀선의 모양에 영향을 끼치는 점들 위치)

plot(m, which = c(4, 6))
# 순서별 쿡의 거리
# 레버리지와 쿡의 거리

# 회귀 직선의 시각화
plot(cars$speed, cars$dist)
# 선형회귀 라인 그리기
abline(coef(m))
summary(cars$speed)
# 신뢰구간을 구한다.
predict(m,
        newdata = data.frame(speed = seq(4.0, 25.0, 0.2)),
        interval = "confidence")
speed <- seq(min(cars$speed), max(cars$speed), 0.1)
speed
ys <- predict(m, data.frame(speed = speed), interval = "confidence")
matplot(speed, ys, type = 'n')
matlines(speed, ys , lty = c(1, 2, 2), col=1)

# 03 중선형 회귀
(m <- lm(data = iris, Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width))
summary(m)

(m <- lm(Sepal.Length ~ . , data = iris))
# 범주형 변수 Species를 다음과 같이 2개의 가변수를 사용해 표현했기 때문이다.
summary(m)

model.matrix(m)[c(1,51,101),]

anova(m)

# 중선형 회귀 모델의 시각화
with(iris, plot(Sepal.Width, Sepal.Length,
                cex = .7,#점의 크기
                pch = as.numeric(Species)))# 점의 형태
as.numeric(iris$Species)

m <- lm(data = iris, Sepal.Length ~ Sepal.Width + Species)
coef(m)

abline(2.25 , 0.80, lty = 1)
abline(2.25 + 1.45, 0.80, lty = 2)
abline(2.25 + 1.94, 0.80, lty = 3)
legend("topright", levels(iris$Species), pch = 1:3 , bg = "white")

# 표현식을 위한 I()의 사용
x <- 1:1000
y <- x^2 + 3 * x + 5 + rnorm(1000)
lm(y ~ I(x^2) + x)
lm(y ~ x^2 )

x1 <- 1:1000
x2 <- 3 * x1
y <- 3 * (x1 + x2) + rnorm(1000)
lm(y ~ I (x1 + x2))
lm(y ~ x1 + x2)

# 변수의 변환
x <- 101:200
y <- exp(3 * x + rnorm(100))
lm(log(y) ~ x)

x <- 101:200
y <- log(x) + rnorm(100)
lm(y ~ log(x))

# 상호 작용
data("Orange")
Orange
with(Orange,
     plot(Tree, circumference, xlab = "tree", ylab = "circumference"))
with(Orange,
     interaction.plot(age, Tree, circumference))
# 나무의 수령이 높아짐에 따라 둘레가 길어지는 추세가 관찰되었다.

# 
Orange[, "fTree"] <- factor(Orange[, "Tree"], ordered = F)
m <- lm(circumference ~ fTree * age , data = Orange)
anova(m)
head(model.matrix(m))
mm <- model.matrix(m)
mm[, grep("age", colnames(mm))]

# 04 이상치
data("Orange")
m <- lm(circumference ~ age + I(age^2), data= Orange)
rstudent(m)
library("car")
outlierTest(m)
data("Orange")
Orange <- rbind(Orange,
                data.frame(Tree=as.factor(c(6,6,6)),
                           age = c(118,484,664),
                           circumference = c(177,50,30)))
tail(Orange)
m <- lm(circumference ~ age + I(age^2), data= Orange)
outlierTest(m)
# 36번 데이터의 문제

# 05 변수 선택
# 변수 선택 방법
# 1. 전진 선택법
# 2. 변수 소거법
# 3. 단계적 방법(stepwise)

# install.packages("mlbench")
library("mlbench")
data("BostonHousing")
head(BostonHousing)
m <- lm(medv ~ ., data=BostonHousing)
m2 <- step(m, direction = "both")
formula(m2)
step(m, direction = "both")
lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
     tax + ptratio + b + lstat, data = BostonHousing)

# 모든 경우에 대한 비교
install.packages("leaps")
library("leaps")
library(mlbench)
data("BostonHousing")
m <- regsubsets(medv ~ ., data=BostonHousing)
summary(m)
summary(m)$bic
summary(m)$adjr2
plot(m, scale="adjr2")
plot(m, scale="bic")
