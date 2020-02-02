# chapter4_2
# ###############################################
# 작성자 : 이희빈
# 작성일 : 2020-01-30
# ###############################################


##4_2_6 데이터 분리 및 병합
##1. split() 조건에 따른 데이터 분리
## iris 종별 Sepal.Length의 평균
split(iris, iris$Species)
lapply(split(iris$Sepal.Length, iris$Species), mean)
## tapply(vector, index, fun)와 비교
tapply(iris$Sepal.Length, iris$Species, mean)

##2. subset() 조건을 만족하는 특정 부분만 반환
## setosa종만 뽑기
subset(iris, Species == "setosa")
## 조건 추가
subset(iris, Species == "setosa" & Sepal.Length > 5.0)
## 특정컬럼 선택/제외
subset(iris, select=c(Sepal.Length, Species))
subset(iris, select=-c(Sepal.Length, Species))
## + names()와 %in%를 사용하여 특정 컬럼 제외하기
iris[,!names(iris) %in% c("Sepal.Length", "Species")]

##4. merge() 두 데이터 프레임을 공통된 값을 기준으로 묶음
x<-data.frame(name=c("a", "b", "c"), math=c(1,2,3))
y<-data.frame(name=c("c", "b", "a"), english=c(4,5,6))
merge(x, y)
## 수행 시, 공통된 값이 한쪽에만 있는 경우,
y<-data.frame(name=c("d", "b", "a"), english=c(4,5,6))
merge(x, y)
merge(x, y, all=TRUE)

#4_2_7 데이터 정렬
##1. sort() 벡터 정렬
x<-c(20, 11, 33, 50, 47)
sort(x)
sort(x, decreasing=TRUE)

##2. order() 정렬하기 위한 각 요소의 색인을 반환
order(x)
order(x, decreasing = TRUE)
## 위치값 전달하므로 데이터 프레임 정렬에 활용
iris[order(iris$Sepal.Length), ]
iris[order(iris$Sepal.Length, iris$Sepal.Width), ]

#4_2_8_데이터 프레임 컬럼 접근
##1. with() 데이터 프러에미 또는 리스트 내 필드 이름만으로 접근
with(iris, {
 print(mean(Sepal.Length)) 
 print(mean(Sepal.Width))}
)

##2. within() 데이터 수정 기능 추가
x<-data.frame(val=c(1,2,3,4,NA,5,NA))
x<-within(x, {
  val<-ifelse(is.na(val), median(val, na.rm=TRUE), val)
})
## 아래 코드 결과와 같음
x$val[is.na(x$val)] <-median(x$val, na.rm = TRUE)
## +응용: 종의 중앙값으로 iris 내 결측치 변경
median_per_species <-sapply(split(iris$Sepal.Length, iris$Species), median, na.rm=TRUE)
iris<-within(iris, {
  Sepal.Length <- ifelse(is.na(Sepal.Length), median_per_species[Species], Sepal.Length)
})

##3. attach()함수 호출 후 모든 코드에서 필드 이름만으로 접근할 수 있음
## -- 데이터를 R 검색 경로에 추가되어 이름으로 데이터를 곧바로 접근
##    단, attach() 후 이뤄진 변수 수정은 detach() 시 원래의 데이터 프레임에 반영되지 않음
attach(iris)
head(Sepal.Width)
## head(iris$Sepal.width)와 동일한 결과
detach(iris)
head(Sepal.Width) ## Error

#4_2_9 조건에 맞는 데이터의 색인 찾기
##1. which() 조건이 참인 색인 반환
## 주의: subset(iris, Species =="setosa") 또는 iris[iris$Species =="Setosa", ]는 행반환
which(iris$Species== "Setosa")
which.min(iris$Sepal.Length)
which.max(iris$Sepal.Length)

#4_2_10 그룹별 연산
##1. aggregate() 데이터를 그룹으로 묶은 후 임의의 함수 적용 가능
aggregate(Sepal.Width ~ Species, iris, mean)
tapply(iris$Sepal.Width, iris$Species, mean)

#4_2_11. 편리한 처리를 위한 데이터의 재표현
## tidy data(value와 category로 구성) 
##특징 : 각 변수는 하나의 컬럼에 해당, 한 행은 하나의 관찰,
##      한 관찰 유형은 하나의 테이블 형성
##stack(), unstack() 스프레트시트 데이터 형태와 tidy data 형태 변환 함수
x<-data.frame(a =c(3,2,9),
              b =c(5,3,2),
              c=c(4,5,7))
summaryBy(values~ind, x_stacked)
unstack(x_stacked, values~ind)

#4_2_12 MySql 연동
install.packages("RMySQL", type ="Source")
library(RMySQL)
con<-dbConnect(MySQL(), user ="유저명", password="비밀번호",
               dbname="DB명", host="ip주소")
dbListTables(con)
dbGetQuery(con, "Select * from score")

