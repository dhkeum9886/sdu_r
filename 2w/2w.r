a=4
str(iris)
iris
iris $ Species
summary(iris [,5]) 
iris [,3:5]
iris $Sepal.Length
iris $Species
tmp<-as.character(iris[,5])
str(tmp)
summary(tmp)
setwd("D:/Project/sdu_r")
write.csv(iris, "iris.csv")
tmp

available.packages()
install.packages("PerformanceAnalytics") # 라이브러리 코멘드까지 필요함
library(PerformanceAnalytics)            # 설치후 반드시 실행해야됨.
iris
chart.Correlation(iris[,1:4], histogram=, pch="+")  # 시각화화

# 중앙값 median(객체$변수)
# 평균   mean
# 분산• 관찰값들이 평균(mean)으로부터 얼마나
#        퍼져있는지를 나타내는는 값
#      • 분산이 클수록 자료의 값들의 변동이
#         크다고 할 수 있다.
#      • var(객체$변수)
# 표준편차 • 표준편차는 분산의 제곱근
#          • sd(객체$변수)
# 상관계수 • cor(객체$변수)
Var1<-c(1,2,2,2,3,3,4)
Var2<-c(3,7,8,8,9,9,4000)

summary(Var1)
summary(Var2)

var(Var1)
var(Var2)

sd(Var1)
sd(Var2)

cor(Var1, Var2)

plot(Var1)
plot(Var2)
plot(Var1, Var2)
boxplot(Var1)
boxplot(Var2)
boxplot(Var1, Var2)

# scale : 데이터 집합의 전체 표준 편차를 1로 만듦
tmp<-data.frame(Var1,Var2)
tmp
tmp1<-scale(tmp)
tmp1
boxplot(tmp1)


#  caret(C_lassification And REgression Training) 패키지
install.packages("caret")
library(caret)
head(mtcars) ; str(mtcars)
mtcars$cyl<-as.factor(mtcars$cyl)
str(mtcars) ; head(mtcars)

prep <- preProcess(mtcars, c("center", "scale"))
tmp <- predict(prep, mtcars)
head(tmp)


# 시각화 패키지
install.packages("ggplot2")
library(ggplot2)
View(diamonds)
str(diamonds)
summary(diamonds)
cor(diamonds)
cor(diamonds[,-(2:4)])
cor(diamonds[,c(1,5:10)]))
cnt=length(diamonds)
cor(diamonds[,c(1,5:cnt)])

qplot(diamonds$carat, diamonds$price)
qplot(carat,price, data=diamonds)
qplot(data=diamonds, x=carat, y=price)
qplot(data=diamonds, x=carat, y=price, geom="point")
qplot(diamonds$price,diamonds$depth)
qplot(carat, price, data = diamonds, geom=c("point","smooth"), method=lm,color=cut)

ggplot(data=diamonds,aes(x=cut))+geom_bar()
ggplot + geom_point()

g=ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width))
p=geom_point(aes(color=Species),size=7, alpha=0.5)
g+p

p = ggplot(iris, aes(Species, Sepal.Length))
p + geom_boxplot()+geom_jitter(width = 0.2)
