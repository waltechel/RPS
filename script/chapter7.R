# chapter7
# ###############################################
# 작성자 : 김현정
# 작성일 : 20200216
# ###############################################

#1번
#난수생성 및 분포 함수
#rnorm(난수, 평균, 표준편차)
rnorm(100, 0, 10)
#density 밀도그림
plot(density(rnorm(1000000,0,10)))

#포아송분포의 확률질량함수
dpois(3,1)
(1^3*exp(-1))/(factorial(3))

#N(0,1)의 정규 분포에서 누적분포 F(0)
#그리고 50%에 대한 분위수 F^-1*(0.5)를 구해보자
#누적분포
pnorm(0)
pnorm(1.96)
pnorm(2.58)
#분위수
qnorm(0.5)
qnorm(0.25)
qnorm(0.99)
qnorm(0.95)
qnorm(0.975)

#2번 기초통계량
#mean : 평균
#mean(x, trim =, na.rm=FALSE, ...)
#var  : 표본분산
#sd   : 표본 표준 편차
mean(1:5)
var(1:5)
sum((1:5-mean(1:5))^2)/(5-1)
sd(1:5)

#다섯수치요약
#최소, 제1사분위수, 중앙값, 제사분위수, 최대값
#fivenm(x, na.rm=TRUE)
fivenum(1:11)
summary(1:11)
#그러나 fivenum과 summary는 제1사분위수와 제3사분위수를 찾는방법이 다르다.
fivenum(1:4)
summary(1:4)

#최빈값
#table 분할표를 작성한다.
#which.max 최대값이 저장된 위치에 색인을 반환한다.

(x <- factor(c("a", "b", "c", "c", "c", "d", "d")))
table(x)
which.max(table(x))
#해당위치에 있는 셀의 이름
names(table(x))[3]
names(table(x))[2]
names(table(x))[1]

#단순임의추출
#sample (x, size, replace = FALSE, prob(c(5,2,3)), prob=NULL)
sample(1:10,5)
sample(1:10,5 , replace = TRUE)

#가중치를 고려한 표본추출
#1에서 10까지의 수에 가각 1에서 10까지가중치를 주어 복원 추출 해보자.
sample(1:10, 5, replace = TRUE, prob=1:10)

#층화 임의 추출
#sampling::strata
#sampling::strata(data, stratanames=NULL,size, #method c("srswor", "srswr", "poisson", "systematic"), pik,
#                  description = FALSE)

#sampling::getdata(data, m #선택된 유닛에 대한 벳터 또는 표본 데이터 프레임)
# install.packages("sampling")
library(sampling)

#오류출력
strata(c("Species"), size = c(3, 3, 3), method= "srswor", data= iris) #비복원단순임의추출

strata(c("Species"), size=c(3, 1, 1), method= "srswr", data=iris) #복원단순임의추출

y <- strata(c("Species"), size=c(3, 1, 1), method= "srswr", data=iris)

getdata(iris,y)

#Species2의 각 층마다 1개씩 표본을 추출
iris$Species2 <- rep(1:2, 75) # 1,2,1,2,1,2....1,2
strata(c("Species", "Species2"), size = c(1,1,1,1,1,1), method = "srswr", data=iris)

#계통추출
(x <- data.frame(x=1:10))
# install.packages("doBy")
library(doBy)
x
sampleBy(~1,frac=.3, data=x, systematic = TRUE)
XX <- sampleBy(~1,frac=.1, data=iris, systematic = TRUE)
XX
str(XX)

#분할표
#xtabs(formula,p data,)
table(c("a", "b", "b", "b","c","c", "c", "d"))

d <- data.frame(x=c("1","2", "2", "1"),
                y=c("A","B", "A", "B"),
                num=c(3,5,8,7))
d
(xtabs(num~x+y, data=d))
(xtabs(num~y+x, data=d))

(d2 <- data.frame(x=c("A", "A", "A", "B", "B")))
(xtabs(~x,d2))

#합,비율의 계산
#margin.table
#prop.table

xt <- data.frame(x=c("1", "2", "1", "2"),
                 y=c("A", "A", "B", "B"),
                 num=c(3,8,7,5))
xt <- (xtabs (num ~ x+y, data=xt))
xt
margin.table(xt,1) # 3+7 =10 , 8+5 = 13
margin.table(xt,2) # 3+8 =11 , 7+5 = 12
margin.table(xt) # 3+8+7+5 = 23

xt
prop.table(xt,1) # 10, 13 행별로 나눈 값
prop.table(xt,2) # 11, 12 열로 나눈 값
prop.table(xt)


#독립성 검정
#P(i,j) = p(i)*p(j)
# install.packages("MASS")
library(MASS)
data(survey)
str(survey)

head(survey[c("Sex", "Exer")])
xtabs(~Sex + Exer, data=survey)
#카이제곱검정 #0.05보다 크다. 그러므로 H0을 기각하지 못한다. 즉 독립이다.
chisq.test(xtabs(~Sex + Exer, data= survey))

#피셔의 정확 검정
#카이제곱검정이 부정확한 경우에는 피셔의 정확검정을 사용한다.
xtabs(~W.Hnd + Clap, data = survey)
#경고메세지가 뜨면 피셔의정확검정으로 넘어간다.
chisq.test(xtabs(~W.Hnd + Clap, data=survey))
#p-value가 0.05보다 작음으로 귀무가설을 기각하고 대립가설을 채택한다.
fisher.test(xtabs(~W.Hnd + Clap, data=survey))

#맥니마 검정
Performance <- matrix(c(794,86,150,570), nrow =2 ,
                      dimnames = list("1st" = c("YES", "NO"),
                                      "2st" = c("YES", "NO")))
Performance

#지지율에 변화가 있었다.
mcnemar.test(Performance)
# 86이 86+150이 절반에 해당하는지여부를 이항분포로분석했더니 p값이 0.05보다 작아서 기각
binom.test(86, 86+150, 0.5)

#적합도 검정
table(survey$W.Hnd)
chisq.test(table(survey$W.Hnd), p=c(.3,.7))

#샤피로 윌크 검정
#데이터가 정규분포를 따르는지 샤피로 윌클 검정을 수행한다. 귀무가설은정ㅈ규분포를 따른다는것이다.
shapiro.test(rnorm(1000))

#콜모고로프 스미르노프 검정(k-s검정)
ks.test(rnorm(100), rnorm(100)) # 같은분포이다.
ks.test(rnorm(100), runif(100)) # runif 균등분포 # 다른분포이다.
ks.test(rnorm(1000), "pnorm", 0,1)

#Q-Q도
#qqnorm : 주어진 데이터와 정규 확률분포를 비교하는 Q-Q도를 그린다.
#qqplot : 두 데이터 셋에 대한 Q-Q도를 그린다.
#qqline : 데이터와분포를 비교해 이론적으로 성립해야 하는 직선 관계를 그린다.
x <- rnorm(1000, mean = 10 , sd =1)
qqnorm(x)
qqline(x, lty =2)

x <- runif(1000)
qqnorm(x)
qqline(x, lwd=2)

qqplot(runif(1000, min =1, max = 10 ), 1:10)

#상관분석
#공분산
cov(1:5, 2:6)
cov(1:5, c(3,3,3,3,3))
cov(1:5, 5:1)

cor(iris$Sepal.Width, iris$Sepal.Length)
cor(iris[,1:4])
symnum(cor(iris[,1:4])) # 심볼로 상관계수를 알려준다.

install.packages("corrgram")
library(corrgram)
corrgram(iris[,1:4],upper.panel=panel.conf)

cor(1:10, 1:10)
cor(1:10, 1:10*2)

x=1:10
y=x^3
cor(x,y)

#스피어만 상관 계수
#스피어만 상관계수는 상관계수를 계산할 두 데이터의 실제값 대신 두값의 순위를 사용해 상관계수를 계산하는 방식이다.

x <- c(3,4,5,3,2,1,7,5)
rank(sort(x))

m <- matrix(c(1:10,(1:10)^2), ncol=2)
m
cor(m, method="spearman") #순위계산은 스피어만으로...
cor(m, method="pearson") #피어슨으로 뽑으니 좀 다르게 나옴

#캔달의 순위 상관계수
cor(c(1,2,3,4,5), c(1,0,3,4,5), method="kendall")

#상관계수 검정
#HO:상관계수가 0이다. H1:상관계수가 0이 아니다.
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method="pearson")
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method="spearman")
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method="kendall")

#추정 및 검정
#t.test: 일표본 평균의 구간 추 및 가설 검정
x <- rnorm(30)
x
t.test(x)
x <- rnorm(30, mean = 10)
t.test(x, mu=10) # 모평균이 10인지 검정

#독립이표본
sleep
sleep2 <- sleep[,-3] #3번째 변수를 제거
sleep
sleep2

tapply(sleep2$extra, sleep2$group, mean) #2그룹별 평균산출
library(doBy)
summaryBy(extra ~ group, sleep2)

#분산비에 대한 가설 검정
var.test(extra ~ group, data=sleep2)
#paired = TRUE 짝지은 이표본 FALSE 독립 이표본 var.equal=두집단의 모분산이 같은지 여부
#h0을 기각하지 못함으로 두집단의 모평균에 차이가 없다.
t.test(extra~ group, data =sleep2, paired=FALSE, var.equal=TRUE)

#짝지은 이표본 평균
with(sleep, t.test(extra[group==1], extra[group==2], paired=TRUE))
#0.05보다 작음으로 모평균의 차이가 있다.

#이표본 분산
with(iris, var.test(Sepal.Width, Sepal.Length))

#일표본 비율
#동전을 100번 던졌더니 앞면이 42번 나왔다고 가정
prop.test(42, 100) #정규분포근사에 의한 신뢰구간
binom.test(42, 100) #이항분포로 부터의 신뢰구간

#이표본 비율
#추정 및 검정의 예
# 100번 던져서 45회, 90번 던저서 55회 비율차
# 같은 비율이 아니다.
prop.test(c(45,55), c(100,90))

