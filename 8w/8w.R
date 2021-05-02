setwd('D:/Project/sdu_r/8w')

install.packages("e1071")
library("e1071")

rm(list=ls())  #메모리 자료 모두삭제
ls()

data(iris)

# 메모리에 iris 객체를 올려놓음 attach에서 detach 하기전까지는
# iris$Species 를 Species로 해도 됨
attach(iris)   
ls()

head(Species)

##데이터 추출법1
###무작위 데이터 추출  무작위로 100개 추출
#train_1=sample(1:150,100)

install.packages("caret")
library("caret")


###y값에 맞게 데이터 추출
set.seed(1234) 
intrain=createDataPartition(y=Species, p=0.7, list=FALSE)  #attach안했으면 y=iris$Specie
train=iris[intrain, ]
test=iris[-intrain, ]
str(train)
str(test)

##파라미터 최적값 찾기
#svm함수를 이용하여 gamma, cost 두개의 파라미터의 최적값을 구함
# gamma는 초평면의 기울기, cost는 과적하베 따른 비용
# gamma는 가우시안 함수의 표준편차를 조정하는 인자, 큰값일수록 작은 표준편차가 생김
# kernel이 radial일때는 설정해줘야함. linear일때는 불필요
# gamma의 기본값은 1/(# of dimension)
# 과적합 될수록 cost가 상승함. 어느 정도 비용을 감수하더라도 모델을 훈련 데이터에 맞출지를 설정하는 것임.
# 이상치는 초평면에 많은 영향일때는 제외함
# 주어진 범이내에서 gamma, cost 최적값을 찾아줌


## classification mode
# default with factor response:
model <- svm(Species ~ ., data = train)  ##attach하지 않아도 Species임
#model <- svm(Species ~ ., data = train) 위와 동일함

##여러개의 gamma와 cost값을 사용_시간오래걸리수 있음
model_1 <- tune.svm(Species ~ ., data = train, gamma=2^(-1:1),cost=2^(2:4))
model
print("-----------------------------------")
model_1

##훈련데이터로 모델 생성
##gamma숫자값 클수록 vectors많이 생김
model_2 <- svm(Species ~ ., data = train, gamma=1, cost=16)  ##attach하지 않아도 Species임
model_2

summary(model)
print("----------------------------------------")
summary(model_2)  #gama는1 cost는 16인값임

model$call
model$levels

str(model)

# 예측
#predict(objext, newdata, decision.values=FALSE, probability=FASLE,....na.action=na.omit)
#ovject=> svm, newdata=: An object containing the new input data
#type옵션, reponse(디폴드값으로, 반응값 그대로 반환)
#           probabilities(모델 알고리즘에 따라 확률로 반환)
#           votes(각 분류 가능 범주의 순위를 출력)중 선택가능


##에측한 model을 시각화 support vector는 은 + 모양으로 나타남. 
#### test 데이터 : visualize (classes by color, SV by crosses):
plot(cmdscale(dist(test[,-5])),
     col = as.integer(test[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

#### train데이터 : visualize (classes by color, SV by crosses):
plot(cmdscale(dist(train[,-5])),
     col = as.integer(train[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

###테스트자료에서 svm확인
print(predict(model,test))



#정오분류표(confusion matrix) 작성
##행은 실제 iris, 열은 svm
result=table(test$Species, predict(model, test))
result

정답율=sum(result[row(result)==col(result)])/sum(result)
오답율=1-정답율

print("-------정,오분류----")
print(paste("정답율:", 정답율)) 
print(paste("오답율:", 오답율))



#정오분류표(confusion matrix) 작성
##행은 실제 iris, 열은 svm
result=table(test$Species, predict(model_2, test))
result

정답율=sum(result[row(result)==col(result)])/sum(result)
오답율=1-정답율

print("-------정,오분류----")
print(paste("정답율:", 정답율)) 
print(paste("오답율:", 오답율))



###참고   SVM 모델 저장후 불러오기
#setwd("c:/sdu_r")
#write.svm(model, svm.file = "iris-classifier.svm", scale.file = "iris-classifier.scale")
# read scale file
# the n-th row is corresponding to n-th dimension. The 1st column contains the
# center value, the 2nd column is the scale value.
#tmp=read.table("iris-classifier.scale")
#tmp



###kernel을 바꿔서 모델 생성 및 예측

model_linear <- svm(Species ~ ., data = train, kernel="linear")  ##attach하지 않아도 Species임
model_linear

summary(model_linear)

#### train데이터 : visualize (classes by color, SV by crosses):
plot(cmdscale(dist(test[,-5])),
     col = as.integer(test[,5]),
     pch = c("o","+")[1:150 %in% model_linear$index + 1])


#정오분류표(confusion matrix) 작성
##행은 실제 iris, 열은 svm
result=table(test$Species, predict(model_linear, test))
result

정답율=sum(result[row(result)==col(result)])/sum(result)
오답율=1-정답율

print("-------정,오분류----")
print(paste("정답율:", 정답율)) 
print(paste("오답율:", 오답율))
