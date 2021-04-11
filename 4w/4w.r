setwd('D:/Project/sdu_r/4w')
getwd()
head(mtcars)
str(mtcars)
cor(mtcars$hp, mtcars$mpg)
plot(mtcars$hp, mtcars$mpg)
par(mfrow=c(1,2))  # 화면 1행 2열로 만듬
hist(mtcars$mpg, col='gray')
hist(mtcars$hp, col='gray')
par(mfrow=c(1,2))
par(mfrow=c(1,1))  # 화면 1행 1열로 만듬

lm_user=lm(mpg~hp, data=mtcars)  # lm -----> 회귀분석함수.  문법  mpg > y 축으로  , hp > x 축으로 
lm_user

#Call:
#  lm(formula = mpg ~ hp, data = mtcars)
#
#Coefficients:                #### y= ( -0.06823 * X) + (30.09886 절편.) 
#  (Intercept)           hp  
#30.09886     -0.06823  

summary(lm_user)

#Call:                                            # 사용자가 작성한 회귀식
#  lm(formula = mpg ~ hp, data = mtcars)          
#
#Residuals:                                       # 잔차 ( 오차)
#  Min      1Q  Median      3Q     Max 
#-5.7121 -2.1122 -0.8854  1.5819  8.2360 
#
#Coefficients:                                    # 회귀계수
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 30.09886    1.63392  18.421  < 2e-16 ***
#  hp          -0.06823    0.01012  -6.742 1.79e-07 ***         # 가설검정  * 이 많을수록 회귀계수가 통계적으로 유의미함을 의미
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.863 on 30 degrees of freedom
#Multiple R-squared:  0.6024,	Adjusted R-squared:  0.5892       # 결정계수 1에 가까울수록 강한상관관계 (0.6024)
#F-statistic: 45.46 on 1 and 30 DF,  p-value: 1.788e-07         # 모델 전체의 p-value  이값이 0.05보다 작으면 모델이유의미함

plot(mtcars$hp , mtcars$mpg)
abline(lm_user, col='red')

par(mfrow=c(4,4))               # 각각의 데이터 셋과 mpg 의 연관성을 보려고 함. 
for(i in 1:length(mtcars)) {
  tmp<-mtcars[,i]
  names<-colnames(mtcars)[i]
  plot(tmp, mtcars$mpg, main=names)
  abline(lm_user, col='red')
}

install.packages("ggplot2")
library(ggplot2)
ggplot(data=mtcars, aes(x=hp, y=mpg))+geom_count()+geom_smooth(method="lm")

#hp에 따른 mpg값 예측
predict(lm_user, newdata=mtcars)   


# new hp에 따른 mpg값 예측
hp=c(23,2,3,4)
dd=data.frame(hp)
predict(lm_user, newdata=dd)


x1=c(1,1,3,5,6,7,9,8,9,12)
x2=c(1,5,9,25,40,60,65,70,74,80)
x3=c(208,20,1,20,30,50,70,90,200,100)
y=c(10,20,30,40,50,60,70,80,90,100)
data=data.frame(x1,x2,x3,y) 
cor(data)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(data, histogram=, pch="+") #빨간그래프차트


#트레이닝, 테스트 데이터 나눔
n = nrow(data) ; set.seed(200) #seed는 random시 고정하는 번호임

##비복원추출(F) , 복원추출(T)
ind = sample(1:n, n*0.7, replace = F) #전체10개중 70%       # 무작위로 샘플 데이터 추출
train = data[ind,] ; test = data[-ind,] #70%외 나머지

dim(train) #결과는 7,4
dim(test) #결과는 3, 4


model=lm(y~.,data=train) #model=lm(y~x1+x2+x3.data=train)
summary(model)

install.packages("car")
library(car)
##분산 평창지수 10이상이면 강한 상관성, 삭제하고 해야함
vif(model) #vif(lm(y~.,data=db))

#새로운 모델 작성
train.1=data.frame(x3=train$x3,y=train$y)
model.1=lm(y~x3,data=train.1)
summary(model.1)

#모델값중 이상치 제거하고 새로 lm

# train.1
outlierTest(model.1)  # 7번 로우가 이상치로 출력

# 7번 로우 이상치 제거 
train.1=subset(train.1,rownames(train.1) !="7")
model.1=lm(y~x3,data=train.1)
summary(model.1)


outlierTest(model.1) # 2번 로우가 이상치
# 2번 로우 이상치 제거
train.1=subset(train.1,rownames(train.1) !="2")
model.1=lm(y~x3,data=train.1)
summary(model.1)

Val.X <- c(46,47,58,47,27,58,56,26,47,25)
Val.Y <- c(78,57,31,28,67,77,36,57,36,57)
var.test(Val.X, Val.Y)

Val.X <- c(1, 46,47,58,47,27,58,56,26,47,25)
Val.Y <- c(1, 78,57,31,28,67,77,36,57,36,57)
var.test(Val.X, Val.Y)
