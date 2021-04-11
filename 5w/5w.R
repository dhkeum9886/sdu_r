setwd('D:/Project/sdu_r/5w')

# 로지스틱 회귀
rm(list=ls())
str(iris)
data1=iris

set.seed(1000)  #랜덤값 발생시 seed번호로 같은 값 나오게 함
##sample함수는 무작위 추출함수이며 replace=F를 써서
##같은 값을 두번 추출하지 않는 비복원 추출방법을 사용
##같은값을 여러번 추출하게 하려면 복원추출법인 replace=T 사용
ind = sample(1:nrow(iris),nrow(iris)*0.7,replace = F)

train = data1[ind, ] ##트레이닝 데이터  150*0.7=105개
test = data1[-ind, ] ##테스트데이터, 트레이닝 데이터 외(-표시) 150*0.3=45개

str(train)
str(test)

print("############ train #############")
head(train)

print("############ test #############")
head(test)

##로지스틱회귀 glm, 꽃의 품종을 꽃받침너비높이, 꽃잎너비높이 4개변수와 회귀
model = glm(Species~., data = train, family = "binomial")
model$fitted[1:4]    #예상치

str(model)
str(model$fitted)


train_y = ifelse(model$fitted.values>=0.5, 2, 1)
train_y[1:4]
table(train_y)   ##값의 개수를 분류별로 집계함

table(train$Species, train_y)


##45개의 테스트 데이터를 통한 모델 확인
pred1 = predict(model, newdata = test, type = 'response')
pred1

pred_label = ifelse(pred1 >= 0.5, 2, 1)
table(test$Species, pred_label)

test$label = pred_label
test

install.packages("nnet")
library(nnet)

set.seed(1001)
ind = sample(1:nrow(iris),nrow(iris)*0.7,replace = F)
train = iris[ind, ]
test = iris[-ind, ]

m = multinom(Species~., data=train)
m$fitted
m_class = max.col(m$fitted.values)

table(m_class)
table(train$Species, m_class)

pred3 = predict(m, newdata = test, type = 'class')
# class는 factor 결과값을 구해준다.
table(pred3)
table(test$Species, pred3)
