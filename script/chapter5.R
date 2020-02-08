# chapter5
# ###############################################
# 작성자 : 이동준 
# 작성일 : 20200208
# ###############################################
rm(list = ls())

# 5장 데이터 조작 2: 데이터 처리 및 가공

# 01 데이터 처리 및 가공 패키지
# sqldf : sql
# plyr : split, apply, combine
# reshape2 : melt, dcast 
# data.table : fread
# foreach : for
# doParallel
# testthat : 유닛 테스팅 프레임워크

# 02 sql을 사용한 데이터 처리
install.packages("sqldf")
library(sqldf)
sqldf("select distinct Species from iris")

sqldf("select avg(Sepal.Width) from iris where Species ='setosa'")
head(iris)

# R과 달리 sql에서 .은 컬럼명이 될 수 없으므로 Sepal.Length가 아니라
# Sepal_Length로 컬럼명을 적어야 한다. 또한 SQL에서 대소문자 구별은 없으므로 Sepal_Length 대신 sepal_length로 적어도 된다.
colnames(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
sqldf("select avg(Sepal.Width) from iris where Species ='setosa'")

colnames(iris) <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species")
sqldf("select avg(Sepal_Length) from iris where Species ='setosa'")

mean(subset(iris, Species == "setosa")$Sepal_Length)

# R에 아직 익숙하지 않고 SQL에 익숙한 사용자들에게는 sqldf가 확실히 편한 인터페이스를 제공한다.
sqldf("select species, avg(sepal_length) from iris group by species")
sapply(split(iris$Sepal.Length, iris$Species), mean)
split(iris$Sepal.Length, iris$Species)

# 03 분할 적용, 재조합을 통한 데이터 분석
# ply
# adply : 배열
# ddply : 데이터 프레임
# ldply : 리스트
install.packages("plyr")
library(plyr)

apply(iris[, 1:4], 1, function(row){print(row)})
# 문자열이 섞이면 데이터가 모두 문자열로 변환된다.
apply(iris, 1, function(row){print(row)})

# 새로운 컬럼 V1에 기록한다.
adply(iris, 
      1, 
      function(row){row$Sepal.Length >= 5.0 &
          row$Species == "setosa"
      })

adply(iris, 
      1, 
      function(row){
          data.frame(
            sepal_ge_5_setosa = c(row$Sepal.Length >= 5.0 &
                                    row$Species == "setosa")
          )
      })

# ddply() : .variables에서 나열한 컬럼에 따라 데이터를 나눈 뒤 함수를 적용한다는 점이다. 
# ddply(
# .data,
# .variables, 
# .fun = 
# )
ddply(iris,
      .(Species),
      function(sub){
        sepal.width.mean = mean(sub$Sepal.Width)
      })

ddply(iris,
      .(Species),
      function(sub){
        data.frame(sepal.width.mean = mean(sub$Sepal.Width))
      })

ddply(iris,
      .(Species, Sepal.Length > 5.0), 
      function(sub){
        data.frame(sepal.width.mena = mean(sub$Sepal.Width))
      })

# baseball 데이터를 활용한 예를 살펴보자.
head(baseball)
head(subset(baseball, id=="ansonca01"))
# g : 각 선수가 출전한 게임 수
# 곧 각 선수마다의 평균 게임 출전 수를 나타내는 R
head(ddply(baseball, .(id), function(sub){
  mean(sub$g)
}))

# 그룹마다 연산을 쉽게 수행하기
# transform
# 커리어연도 확인하기
head(ddply(baseball, .(id), transform, cyear = year - min(year) + 1))

# mutate
# 앞서 추가한 컬럼을 뒤에 추가하는 컬럼에서 참조할 수 있어 편리하다.
head(ddply(baseball, .(id), mutate,
           cyear = year -min(year) + 1, 
           loy_cyear = log(cyear)))
# summarise()
head(ddply(baseball, .(id), summarise, minyear = min(year)))
head(ddply(baseball, .(id), summarise, minyear = min(year), maxyear = max(year)))

# subset
# 각 아이디별로 게임 수가 가장 많은 해의 기록을 나타낸다.
head(ddply(baseball, .(id), subset, g==max(g)))

# mdply
(x <- data.frame(mean = 1:5, sd = 1:5))
# 데이터프레임의 각 행을 rnorm 함수의 mean, sd에 대한 인자로 넘겨주어 실행한 뒤
# 그 결과를 데이터 프레임으로 모을 수 있다.
mdply(x, rnorm, n = 2)

# 04. 데이터 구조의 변형과 요약
# melt() : 여러 컬럼으로 구성된 데이터를 id, variable, value 라는 세 개 컬럼으로 변환한다.
# cast() : melt 된 데이터를 다시 여러 컬럼으로 변환한다.
# cast 시 melt된 데이터의 요약값을 자동으로 계산해주는 기능이 있다.

install.packages("reshape2")
library("reshape2")

head(french_fries)
# data description
# ?french_fries
# time in weeks from start of study.
# treatment (type of oil),
# subject,
# replicate 몇 번째 응답인가를 묻고 있다.
# potato-y flavour,
# buttery flavour,
# grassy flavour,
# rancid flavour,
# painty flavour

# time, treatment, subject, rep를 놓고 봤을 때
# 실험한 결괏값
m <- melt(french_fries, id.vars = 1:4)
head(m)

library(plyr)
ddply(m, .(variable), summarise, mean = mean(value))
ddply(m, .(variable), summarise, mean = mean(value, na.rm = T))
french_fries[!complete.cases(french_fries),]

# cast()
# dcast() 결과로 데이터 프레임을 반환하며, acast()는 벡터, 행렬, 배열을 반환한다.
m <- melt(french_fries, id.vars = 1:4)
head(m)
# 앞 부분이 id 변수라는 점에 주의한다.
r <- dcast(m, time + treatment + subject + rep ~ ... )
rownames(r) <- NULL
rownames(french_fries) <- NULL
identical(r, french_fries)

m <- melt(french_fries, id.vars = 1:4)
head(m)
# time에 따라 variable 이 어떻게 바뀌는지를 파악
dcast(m , time ~ variable)

# time에 따라 variable 의 평균을 추출
dcast(m, time ~ variable, mean , na.rm = T)

# time에 따라 (treatment, variable) 순서쌍에 해당하는 value의 평균을 추출
head(dcast(m, time ~ treatment + variable , mean, na.rm = T))
# 똑같은 내용을 ddply 함수로 구현
head(ddply(m, .(time, treatment, variable), function(rows){
  return(mean(rows$value, na.rm = T))
}))


# 05 : 데이터 테이블 : 더 빠르고 편리한 데이터프레임
install.packages("data.table")
library(data.table)
(iris_table <- as.data.table(iris))
(x <- data.table(x = c(1,2,3), y = c("a", "b", "c")))
# table 목록 열람
tables()

# 데이터 접근과 그룹 연산
dt <- as.data.table(iris)
dt[1,]
dt[dt$Species == "setosa",]
dt[1, Sepal.Length]
dt[1, list(Sepal.Length, Species)]
dt[, mean(Sepal.Length)]
dt[, mean(Sepal.Length - Sepal.Width)]
dt[, mean(Sepal.Length), by = "Species"]

dt <- data.table(x = c(1,2,3,4,5),
                 y = c("a", "a", "a", "b", "b"),
                 z = c("c", "c", "d", "d", "d"))
dt
dt[, mean(x), by = "y,z"]

# key 값을 사용한 빠른 데이터 접근
df <- data.frame(x = runif(260000), y = rep(LETTERS, each=10000))
head(df)
str(df)
system.time(x <- df[df$y=="C",])
dt <- as.data.table(df)
setkey(dt, y)
# 색인을 이용한 검색에서는 J()함수를 사용한다.
system.time(x <- dt[J("C")])
# y 값이 c인 행들을 찾은 뒤 x의 평균을 구한 결과다.
dt[J("C"), mean(x)]
dt[J("C"), list(x_mean =  mean(x), x_std = sd(x))]

# key를 사용한 데이터 테이블 병함
dt1 <- data.table(x = runif(260000),
                  y = rep (LETTERS, each = 10000))
dt2 <- data.table(y = c("A", "B", "C"),
                  z = c("a", "b", "c"))
head(dt1)
head(dt2)
# dt1[dt2,]는 dt1으로부터 y 값이 A, B, C인 행을 찾아서 병합한다.
setkey(dt1, y)
dt1[dt2, ]
setkey(dt2, y)
dt2[dt1, ]

system.time(dt1[dt2,])

df1 <- as.data.frame(dt1)
df2 <- as.data.frame(dt2)
system.time(merge(df1, df2))

# 참조를 사용한 데이터 수정
m <- matrix(1, nrow = 1000, ncol = 100)
df <- as.data.frame(m)            
dt <- as.data.table(m)

system.time({
  for(i in 1:1000){
    df[i, 1] <- i
  }
})
# 데이터 테이블의 경우 이렇게 쓸 수도 있다.
system.time({
  for(i in 1:1000){
    dt[i, V1:=i]
  }
})

# 리스트를 데이터 프레임으로 빨리 변환하기
# rbindlist() 함수를 사용하여 리스트를 데이터 프레임으로 빨리 변환하는 방법도 있다.
system.time(x <- ldply(1:10000, function(x){
  data.frame(val = x,
             val2 = 2 * x,
             val3 = 2 / x,
             val4 = 4 * x,
             val5 = 4 / x)
  
}))

system.time(x <- llply(1:10000, function(x){
  data.frame(val = x,
             val2 = 2 * x,
             val3 = 2 / x,
             val4 = 4 * x,
             val5 = 4 / x)
  
}))

system.time(x <- rbindlist(x))

# 06 더 나은 반복문
install.packages("foreach")
library("foreach")

# .combine()이 지정되지 않았으므로 결과는 list 형식으로 출력된다.
foreach(i = 1:5)%do%{
  return (i)
}
# .combine = c 를 지정하면 결과를 벡터로 받는다.
foreach(i = 1:5, .combine = c) %do% {
  return(i)
}
# .combine = rbind 를 지정하면 결과를 행 방향으로 받는다.
foreach(i = 1:5, .combine = rbind) %do% {
  return(i)
}
# .combine = cbind 를 지정하면 결과를 행 방향으로 받는다.
foreach(i = 1:5, .combine = cbind) %do% {
  return(i)
}
# .combine = "+"를 지정하면 모든 결과를 합친 결과를 받는다.
foreach(i = 1:5, .combine = "+") %do% {
  return(i)
}
# .combine = "*"를 지정하면 모든 결과를 합친 결과를 받는다.
foreach(i = 1:5, .combine = "*") %do% {
  return(i)
}

# 07 병렬 처리
# 병렬 처리는 adp 및 통계 시험에 필요가 없어서 보지 않습니다.
# doParallel()함수
# registerDoParallel()로 병렬 인자 등록
# 해당 cpu core에 맞게 작업을 하는데 스레싱이나 버퍼 오버를 주의하도록 한다.
# 병렬처리에 자신이 없으면 병렬처리를 안 하는 것도 좋다.

# 08 유닛 테스팅과 디버깅
# testthat
install.packages("testthat")
library(testthat)

a <- 1:3
b <- 1:3
expect_equal(a, b)
expect_equivalent(a, b)
names(a) <- c('a', 'b', 'c')
expect_equal(a, b) #테스트 실패
expect_equivalent(a, b)# 벡터에 부여한 이름을 무시하므로 테스트 성공

fib <- function(n){
  if(n == 0){
    return (1)
  }
  if(n > 0){
    return (fib(n-1) + fib(n-2))
  }
}
expect_equal(1, fib(0))
expect_equal(1, fib(1))

# test_that을 사용한 테스트 그룹화
test_that("base case", {
  expect_equal(1, fib(0))
  expect_equal(1, fib(1))
})
fib <- function(n){
  if(n == 0 || n == 1){
    return (1)
  }
  if(n > 0){
    return (fib(n-1) + fib(n-2))
  }
}
test_that("base case", {
  expect_equal(1, fib(0))
  expect_equal(1, fib(1))
})
test_that("recursive case", {
  expect_equal(2, fib(2))
  expect_equal(3, fib(3))
  expect_equal(5, fib(4))
})
# 테스트 실행
# 파일 구조 파악이 중요
source("script/run_tests.R")

# 디버깅
# print, 
# sprintf
# cat
# browser

# print
fib <- function(n){
  if(n == 0 || n == 1){
    print("base case")
    return (1)
  }
  if(n > 0){
    print("recursive case")
    return (fib(n-1) + fib(n-2))
  }
}
fib(1)
fib(4)

# sprintf
sprintf("%d", 123)
sprintf("Number : %d", 123)
sprintf("Number: %d, String: %s", 123, "hello")
sprintf("%.2f", 123.456)
sprintf("%5d", 123)
sprintf("%5d", 1234)
sprintf("%5d", 12345)

# cat
print("hi")
cat("hi")
cat(1,2,3,4,5,"\n")
sum_to_ten <- function(){
  sum <- 0
  cat("Adding ...")
  for(i in 1:10){
    sum <- sum + i
    cat(i, "...")
  }
  cat("done!", "\n")
  return (sum)
}
sum_to_ten()

# browser
sum_to_ten <- function(){
  sum <- 0
  for(i in 1:10){
    sum <- sum + i
    if(i >= 5){
      browser()
    }
     }
  return (sum)
}
sum_to_ten()
# c continue
# n next
# Quit

# 09 코드 수행 시간 측정
# system.time()
sum_to_n <- function(n){
  sum <- 0
  for(i in 1:n){
    sum <- sum + i
  }
  return (sum)
}
system.time(sum_to_n(10000))
system.time(sum_to_n(100000))
system.time(sum_to_n(1000000))
x <- matrix(1:(10000*10000), ncol = 10000)
system.time(save(x, file = "x.RData"))
system.time(load(file = "x.RData"))

# 코드 프로파일링
# Rprof() 는 좀 더 본격적인 코드 수행 성능 평가를 위한 함수다
add_one <- function(val){
  return(val + 1)
}
add_one_to_vec <- function(x){
  for(i in seq_along(x)){
    x[i] <- add_one(x[i])
  }
  return(x)
}
Rprof("add_one.out")
x <- add_one_to_vec(1:1000000)
head(x)
Rprof(NULL)
summaryRprof("add_one.out")
summaryRprof("add_one.out")$by.self
