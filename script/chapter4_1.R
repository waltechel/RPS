# chapter4_1 (p.131-212)
# ###############################################
# ?옉?꽦?옄 :?씠?닔?젙
# ?옉?꽦?씪 :
# ###############################################

### 01 iris data
head(iris)
str(iris)
#'data.frame':	150 obs. of  5 variables:
#str is short of _____

library(help="datasets")
#Version:       3.6.1, Priority:      base, Title:         The R Datasets Package

# 4 ways to see specific dataset info 
# data(mtcars), head(mtcars), ?mtcars, help(mtcars)

### 02 file read/save and out/load
#keyword A: read.csv(), write.csv(), 
#keyword B: save(), load()

#part A
?read.csv

#practice exp.
(x <- read.csv("a.csv"))

#qiuz
a <- read.csv("a.csv")
b <- (read.csv("a.csv"))
# Warning message:In read.table(file = file, header = header, sep = sep, quote = quote,  :'a.csv'에서 readTableHeader에 의하여 발견된 완성되지 않은 마지막 라인입니다
 
str(x)
(x <- read.csv("b.csv"))
names(x) <- c("id", "name", "score")
x
str(x)

(x <- read.csv("b.csv"))
names(x) <- c("id", "name", "score")
x$name <- as.character(x$name)
str(x)  #'data.frame':	2 obs.  How to code?

x <- read.csv("a.csv", stringsAsFactors = FALSE)
str(x)

(c <- read.csv("c.csv"))
str(c)
c <- read.csv("c.csv", na.strings = c("NIL"))
str(c)
is.na(c$score)

write.csv(c, "d.csv", row.names = FALSE) 
read.csv("d.csv")
write.csv(c, "d.csv", row.names = TRUE)
read.csv("d.csv")
#Write means Save

#part B
#keywords: save(), load()
#then what is the diff of Write and Save?
?write.csv
?save

x <- 1:5
y <- 6:10
save(x, y, file = "xy.RData")
list=ls() 
rm(list = ls())
?ls  #short of _________

a <- 1:5
b <- 6:10
c <- 11:15
save(list=ls(), file = "abc.RData")

rm(list = ls())
ls()         #out: character(0)
load("abc.RData")
ls()        #out? 

### 03 bind by row and column
#keywords: rbind(), cbind()

rbind(c(1,2,3),  c(4,5,6))
x <- data.frame(id=c(1,2),  name=c("a","b"), stringsAsFactors = F)

data(x)
#Q) Warning message: In data(x) : 데이터셋 ‘x’을 찾을 수 없습니다

str(x)
y <- rbind(x, c(3, "c"))

cbind(c(1,2,3),  c(4,5,6))
y <- cbind(x, greek=c("alpha", "beta"))
str(y)
y <- cbind(x, greek=c("alpha","beta"), stringsAsFactors=F)
str(y)

### 04 apply() 5 Fuctions p.147
#keywords: apply(), lapply(), sapply(), tapply(), mapply()
?apply
#Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix.
#apply(X, MARGIN, FUN, ...)
# MARGIN, 	a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns. Where X has named dimnames, it can be a character vector selecting dimension names.

sum(1:10)
d <- matrix(1:9, ncol=3)
d

#define Margin, for a matrix 1 indicates rows, 2 indicates columns
apply(d,1,sum) 
apply(d,2,sum)

head(iris)
apply(iris[, 1:4], 2, sum) #== colSums(iris[, 1:4])

#keywords: Lapply(), sapply(), tapply(), mapply()
?lapply() #returns a list of the same length as X
# L is LIST

# NOT List but Vector  -> unlist()
#          but Object? -> do.call()

?unlist #Flatten Lists, Given a list structure x, unlist simplifies it to produce a vector
?do.call #Execute a Function Call, do.call(what, args, 

(result <- lapply(1:3, function(x) {x*2}))
View(result)
result[[1]]

unlist(result)  # is this a real vector? or still a list?   
result
str(unlist(result))
#Quiz)
?vector
is.vector(result)
is.array(result)
is.list(result)

(x <- list(a=1:3, b=4:6))
lapply(x, mean)

lapply(iris[, 1:4], mean)
colMeans(iris[, 1:4])

##Key Point of Type Changes
?unlist() #-  list to vector
?matrix() #-  vector to matrix
?as.data.frame() #-  matrix to data_frame
?names()  #-  get column names(variable names) from list(all Obj) 

#practice1 of Type Changes
d <- as.data.frame(matrix(unlist(lapply(iris[, 1:4], mean)), 
                          ncol = 4, byrow = TRUE))
d
names(d) <- names(iris[, 1:4])
d

#practice2 of Type Changes
data.frame(do.call(cbind, lapply(iris[, 1:4], mean)))
#quiz) ...

#do.call(cbind, lapply(iris[, 1:4], mean)
#a <- do.call(cbind, lapply(iris[, 1:4], mean)
#a <- data.frame(do.call(cbind, lapply(iris[, 1:4], mean)))   

View(a)    
        
x <- list(data.frame(name="foo", value=1),
          data.frame(name="bar", value=2))
x
unlist(x)           
do.call(rbind, x)

#keywords: Sapply(), tapply(), mapply()
?Sapply
#by default returning a vector, matrix
# what is S? __________ as X
lapply(iris[, 1:4],mean) 
sapply(iris[, 1:4],mean)

# sapply and as.data.frame, t()
x <- sapply(iris[, 1:4], mean)
as.data.frame(x)
as.data.frame(t(x))

class(sapply(iris[, 1:4], mean)) 
# [1] "numeric", what does it mean? Class finds out the Type of data
sapply(iris, class)
y <- sapply(iris[, 1:4], function(x){x>3})
class(y)
head(y)
# return of sapply' fun is available only one specific type, no Mix. if there are numeric and factor, then not sapply. use lapply
?tapply #index 
# T means _______
rep(1,10)
tapply(1:10, rep(1,10), sum)
a <- tapply(1:10, 1:10 %% 2 ==1 , sum) #%% _____
a

tapply(iris$Sepal.Length, iris$Species, mean)
#tapply(iris[,1:4], iris$Species, mean)
#Why? the ____ of index, so let's make ____
#exp P.158
m <- matrix(1:8, ncol=2, 
            dimnames = list(c("spr", "sum","fall","win"),
                            c("male","female")))
m
#sum of spr+sum by male/female
indx = list(c(1,1,2,2,1,1,2,2), c(rep(1,4),rep(2,4)))
tapply(m, indx, sum)
#quiz, sum of spr+sum by m+f
indx1 = list(c(1,1,2,2,1,1,2,2))
tapply(m, indx1, sum)

?mapply 
# M is multi,  multivariate version of sapply 
rnorm(10,0,1)

rnorm(1,0,1)
rnorm(2,10,1)
rnorm(3,100,1)
# use mapply
mapply(rnorm,
       c(1,2,3),
       c(0,10,100),
       c(1,1,1))
iris[1:4, 1:4]
mapply(sum, iris[1:4, 1:4] )

### 05 data grouping and functions, doBy package
install.packages("doBy")
library(doBy)
#keywords: summaryBy(), orderBy(), sampleBy()
?summaryBy #formula
summaryBy(Sepal.Width + Sepal.Length ~ Species, iris)

?orderBy 
order(iris$Sepal.Width)
iris[order(iris$Sepal.Width), ]
#formula
orderBy(~ Sepal.Width, iris)[0:4,]
orderBy(~ Species + Sepal.Width, iris)[0:4,]

?sampleBy()
sample(1:10, 5)
nrow(iris)
iris[sample(nrow(iris), nrow(iris)), ]
# 10% of each Species
sampleBy(~ Species, frac = 0.1, data= iris)
#it is not possiblie with sample
sampleBy( ~(Petal.Width > 1.0) , frac = 0.1, data= iris)
# ? 

### 06 split() / subset() and merge()
?split
#split(x, f, drop = FALSE, ...)
#split divides the data in the vector x into the groups defined by f.
split(iris, iris$Species)
#lapply(split(iris, iris$Species), mean)
lapply(split(iris$Sepal.Length, iris$Species), mean)
#lapply(split(iris$Sepal.Length,  , iris$Species), mean)

?subset
subset(iris, Species == "setosa")
subset(iris, Species == "setosa" & Sepal.Length > 5.0) #not && with vector

subset(iris, select = c(Sepal.Length, Species))
subset(iris, select = -c(Sepal.Length, Species)) 
iris [ , !names(iris) %in% c("Sepal.Length", "Species")] #with ""

?merge
x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
merge(x, y, by = c("k1","k2")) # NA's match
merge(x, y, by = "k1") # NA's match, so 6 rows
merge(x, y, by = "k2", incomparables = NA) # 2 rows

### 07 sort and order (are diff)
?sort
?order #order returns a permutation
x <- c(20,11,33,50,47)
sort(x)
order(x)
x[order(x)]
iris[order(iris$Sepal.Length, iris$Sepal.Width), ]
iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = TRUE), ]

### 08 df$colname
# just colname with(), within(), attach(), detach()
?with
print(mean(iris$Sepal.Length))
with(iris, {
  print(mean(Sepal.Length))
  print(length(Species))
  })

?within #For with, the value of the evaluated expr. For within, the modified object.
(x<- data.frame(val=c(1,2,3,4,NA,5,NA)))
x<- within(x, {
  val <- ifelse(is.na(val), median(val, na.rm=TRUE), val)
    })
x
(x<- data.frame(val=c(1,2,3,4,NA,5,NA)))
x$val[is.na(x$val)] <- 10
x

# exp iris
iris[1,1] = NA
median_per_species <- sapply(split(iris$Sepal.Length, iris$Species), median, na.rm=TRUE)
median_per_species
iris <- within(iris, {
  Sepal.Length <- ifelse(is.na(Sepal.Length), median_per_species[Species], Sepal.Length)
  })
iris[1:4, ]

?attach
