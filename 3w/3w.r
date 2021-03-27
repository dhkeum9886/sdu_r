str(iris)
install.packages("tensorflow") ; library(tensorflow)
install.packages("keras") ; library(keras)
install.packages("tidyverse") ; library(tidyverse)
# install.packages("glue") ; library(glue) # 에러 왜인지 모름 ㅜㅜ 
install.packages("forcats") ; library(forcats)
install.packages("timetk") ;library(timetk)
install.packages("tidyquant") ; library(tidyquant)
install.packages("tibbletime") ; library(tibbletime)
install.packages("cowplot") ; library(cowplot)
install.packages("recipes") ; library(recipes)
install.packages("rsample") ; library(rsample)
install.packages("yardstick") ; library(yardstick)
install.packages("fBasics")
library(fBasics)

# iris의 마지막 명목형 데이터를 비교가능한 숫자형 데이터로 변경
rm(list=ls())
str(iris)
summary(iris)
iris[,5] <- as.numeric(as.factor(unlist(iris[,5]))) -1
head(iris)
str(iris)


iris <-matrix(iris) # 숫자만으로 구성된 자료는 matric 로 바꿔서 사용할수있다. 속도가 빠름. 
str(iris)   #매트릭스 형인지 확인
class(iris) #매트릭스 형인지 확인

dimnames(iris)<-NULL
str(iris)
class(iris)

cor(iris)
cor(iris[,1:4])



tmp1=c(2,2,2,2,3,3,3,3,3,4,5,5,20,30,40)
z=tmp1
qplot(tmp1)
s = 2 * z + 4 + rnorm(length(z))
par(mfrow = c(2, 2)) #2행2열
hist(z) ;plot(s, z) ; qqnorm(z)
qqline(z)
plot(1:length(z),log(z), lty = 1)
par(mfrow = c(1, 1))


hist(iris[,3]) ;plot(iris[,3], iris[,3]) ; qqnorm(iris[,3])
hist(iris[1:50,3]) ;plot(iris[1:50,3], iris[1:50,3]) ; qqnorm(iris[1:50,3])

tmp1=c(2,2,2,2,3,3,3,3,3,4,5,5,20,30,40)
tmp2=c(2,2,3,4,40,40,40,50,50,60,60,70,70)
##### 왜도 구하기 ########
skewness(tmp1) #결과값 1.652581
plot(tmp1)
skewness(tmp2) #결과값 -0.3366458
plot(tmp2)
kurtosis(tmp1) #결과값 1.231463
kurtosis(tmp2) #결과값 -1.592747


setwd("D:/Project/sdu_r/3w")
tmp = read.csv("titanic.csv" , stringsAsFactor=TRUE )
str(tmp)
tmp$cabin
levels(tmp$cabin)
ca_Lev=levels(tmp$cabin)
ca_Lev
cnt=length(ca_Lev)
cnt
setwd("D:/Project/sdu_r/3w/titanic")
for(i in 1:cnt){
  val_W=subset(tmp,tmp$cabin==ca_Lev[i])
  file_name=paste(ca_Lev[i],".csv", sep="")
  write.csv(val_W,file_name)
}
levels(tmp$cabin)

stem(tmp$age)
plot(tmp$age)
hist(tmp$age)
boxplot(tmp$age)
summary(tmp)
colSums(is.na(tmp))# NA 몇개인지 확인
na.rm=T
tmp.na.rm=na.omit(tmp)
summary(tmp)

install.packages("Amelia") ; library(Amelia)  # 결측치를 찾아주는 패키지지
missmap(tmp, main = "NA vs. Observed")

missing=data.frame(tmp$pclas,tmp$survived,tmp$age)
summary(missing)

imputed.Missing=amelia(x=missing,m=5)
summary(imputed.Missing)
plot(imputed.Missing)
write.amelia(imputed.Missing, separate=TRUE, "imputed",extension='.csv', format="csv")
str(imputed.Missing)
plot(imputed.Missing[[1]][1][[1]])
plot(imputed.Missing[[1]][1][[1]][,3])
tmp.1 = read.csv("imputed1.csv") ; dim(tmp.1)
tmp.2 = read.csv("imputed2.csv") ; dim(tmp.2)
tmp.3 = read.csv("imputed3.csv") ; dim(tmp.3)
tmp.4 = read.csv("imputed4.csv") ; dim(tmp.4)
tmp.5 = read.csv("imputed5.csv") ; dim(tmp.5)
tmp.re=data.frame(tmp, tmp.1[,4], tmp.2[,4], tmp.3[,4], tmp.4[,4], tmp.5[,4])
str(tmp.re)
summary(tmp.re)

summary(tmp.re$age)
data.frame(tmp$pclas,tmp$survived,tmp$age)
plot(imputed.Missing, which.vars = 1:3)
