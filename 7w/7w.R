setwd('D:/Project/sdu_r/7w')
license()
# 빅데이터 보건복지 공공 복지

#나이브베이즈 패키지 
install.packages('klaR')
library(klaR)

# 시각화 패키지 
install.packages("ggplot2")
library("ggplot2")

# 아이리스 데이터 
str(iris)
summary(iris)
levels(iris$Species)


train=sample(1:150, 100) #무작위로 100개 추출 (학습데이터)

#                      Y ~ X ==> Y 를 결정하는데 있어서 X 를 를 사용하겠다. 
#                     Species ~ .  ==> Species 를 결정하는데 있어서 모든 데이터를 사용하겠당. 
nb_model = NaiveBayes(Species ~., data = iris, subset = train)
nb_model


#학습된 나이브베이즈 모델로 예측
# iris[-train,]   트레인 데이터를 뺀 나머지 
tt = table(iris$Species[-train], predict(nb_model, iris[-train,])$class)
tt

#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)

#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt) #오분류율


test =iris[-train,]
test$pred <- predict(nb_model, iris[-train,])$class #예측된 분류 입력하기
ggplot(test, aes(Species, pred, color = Species))+
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", 
       subtitle="Predicted vs. Observed from Iris dataset", 
       y="Predicted", 
       x="Truth")


df=read.csv("./Heart.csv")
str(df)

# chr 를 factor 로 수정.
df$ChestPain <- as.factor(df$ChestPain)
df$Tha <- as.factor(df$Tha)
df$AHD <- as.factor(df$AHD)

# factor 수정되었나 확인 
str(df)

summary(df)

install.packages("caret")
library(caret)


#트레이닝 테스트 데이터 셋 Y값 비율에 맞추어 나눔
set.seed(1234) # 테스트/트레이닝 데이터를 나누는데 사용될 시드. 
intrain=createDataPartition(y=df$AHD, p=0.7, list=FALSE) 
train=df[intrain, ]
test=df[-intrain, ]

head(train)

summary(train)
summary(test)



# 나이브베이지를 지원하는 다른 패키지 
install.packages("e1071")
library(e1071)

nb_model = naiveBayes(AHD~.,data = train)
nb_model

# 모델성능 확인
nbpred=predict(nb_model, test, type='class')
confusionMatrix(nbpred, test$AHD)
