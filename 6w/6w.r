setwd('D:/Project/sdu_r/6w')


install.packages("class")
library(class)

#연속형변수(1~4, 꽃받침길이,너비, 꽃잎 길이, 너비)
#범주형변수(5, 꽃의종류, setosa, versicolor,virginica), levels(iris[,5])로 확인
str(iris)

# 마지막 분류값은 필요없으므로 iris=iris[,-5]로 마지막 범주형 변수 삭제하고 작업해도 됨
#만약 값의 차이가 많이 날때는 정규화(표준화 작업을 거쳐야함)
#iris<- scale(iris[-4])

par (mfrow = c(2,2))
for(i in 1:4){
  plot(iris[,i],main=colnames(iris)[i],
       col=c("red","blue","green")[unclass(iris[,5])])
}

plot(iris)

str(iris[,1:4])


# 클러스터링의 목적은 세부적으로 데이터를 분석하여 어떻게 분류되는지의 기준을 찾고 
#분류분석에 적용하고자함임
# kmeans(데이터,k값, iter.max=업데이트 회수)

fit=kmeans(iris[,1:4], 3, iter.max=100) 

#fit3.25=kmeans(iris[,1:4],3,nstart=25) #다른 초깃값 25개 시도
fit  #k값3개의 분류1의 개수 62, 분류1의갯수 50, 분류3의 갯수38 개


fit$withinss #클러스터내의 데이터 분산정도: 작을수록(밀집) 클러스터가 잘 구분됨
fit$betweenss #클러스터간 분산정도: 분산이 클수록 클러스터끼리 확연히 분리됨 
fit$totss #withiness + between, totss값이 클수록 분류가 잘되었음을 의미
fit$centers

##군집간의 분산(between)과 군집내에서의 클러스터간 분산(within) 비교
fit=kmeans(iris[,1:4],3, iter.max=100)
fit2=kmeans(iris[,1:4],2, iter.max=100)
fit20=kmeans(iris[,1:4],20, iter.max=100)


##참고: 데이터프레임만들기
fit2_db=data.frame(k=2,betweenss=fit2$betweenss,tot.withiness=fit2$tot.withinss,tot=fit2$totss)
fit3_db=data.frame(k=3,betweenss=fit$betweenss,tot.withiness=fit$tot.withinss,tot=fit$totss)
fit20_db=data.frame(k=20,betweenss=fit20$betweenss,tot.withiness=fit20$tot.withinss,tot=fit20$totss)

fit_value=rbind(fit2_db,fit3_db,fit20_db)

##군집간의 분산(between)이큰가, tot.withiness가 작은가?
fit_value


###클러스터링 적합도 시각화
bet_ss=c()
for(i in 1:100){
  fit.kms=kmeans(scale(iris[,1:4]),i)
  bet_ss[i]=round(fit.kms$betweenss/fit.kms$totss*100,1)
  #bet_ss=rbind(bet_ss,bet_ss[i])  #만약 데이터프레임으로 하려면
}

head(bet_ss,30)
plot(1:100, bet_ss, type="b",ylim=c(0,100),las=1)


Val_error=data.frame()
for(i in 1:100){
  fit.sample=kmeans(iris[,1:4],i)
  tmp=fit.sample$tot.withinss
  Val_error=rbind(Val_error,tmp)
}
print(Val_error)

table(iris$Species, fit$cluster)

par (mfrow = c(2,2))
for(i in 1:4){
  plot(iris[,i],main=colnames(iris)[i],
       #col=c("red","blue","green")[unclass(iris[,5])]
       col=fit$cluster)
  points(fit$centers[,i],col=1:3,pch=8,cex=3)  #8번모양으로 크기는3
}
plot(iris,col=fit$cluster) #변수간 상관관계확인

plot(iris,col=fit2$cluster) #변수간 상관관계확인

#참고
plot(iris[c("Sepal.Length","Sepal.Width")], col=fit$cluster)
points(fit$centers[,c("Sepal.Length","Sepal.Width")],col=1:3,pch=8,cex=2)



#install.packages("cluster")
#library(cluster)
#클러스터링 시각화
clusplot(iris, fit$cluster, main='2차원 평면에 도식화 (k=3)',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#kmeans가 20개일때
fit20=kmeans(iris[,1:4], 20, iter.max=100) 
clusplot(iris, fit20$cluster, main='2차원 평면에 도식화 (k=3)',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#kmeans가 2개일때
fit2=kmeans(iris[,1:4], 2, iter.max=100) 
clusplot(iris, fit2$cluster, main='2차원 평면에 도식화 (k=3)',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# [iris]의 1열 , 중복제거한 유일한 값의 크러스터일 분포 확인 confusion matrix
head(table(iris[,1],fit$cluster))  #head(table(iris[,1],groups))
head(table(iris[,1],fit2$cluster))
head(table(iris[,1],fit20$cluster))


#Elbow Method의 기울기가 완만해지는 점을 적정한 k로 선택하는 함수작성
# 함수작성: Elbow method로 적절한 k 값 찾기  - wss, within sum of squares
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab=paste(i,"of Clusters"),
       ylab="Within group sum of squares")}

par ( mfrow = c(1,2) )
wssplot(iris[,1:4])
wssplot(iris[,1:4], nc = 6)


# 참고: 계층적 클러스터링(Hierarchical clustering) 으로 접근
d <- dist(iris, method = "euclidean") # 유클라디안(Euclidean) 거리 계산법으로
H.fit <- hclust(d, method = "ward.D2") # Ward는 within-cluster variance를 최소화


par (mfrow = c(1,3))
plot(H.fit) # dendogram 도식화
groups <- cutree(H.fit, k = 2) # k=2 활용
rect.hclust(H.fit, k = 2, border = "red")

plot(H.fit) # dendogram 도식화
groups <- cutree(H.fit, k = 20) # k=20 활용
rect.hclust(H.fit, k = 20, border = "red")

plot(H.fit) # dendogram 도식화
groups <- cutree(H.fit, k = 3) # k=3 활용
rect.hclust(H.fit, k = 3, border = "red")

#참고: 최적의 k를 찾아내는 작업
fit_0 = data.frame()   #데이터프레임 생성
for (i in 1:6){
  fit_1 = kmeans(iris[, 1:4], i, iter.max = 100)
  fit_2 = cbind(i, fit_1$tot.withinss)
  fit_0 = rbind(fit_0, fit_2)
  
}
print(fit_0)
plot(fit_0, type = 'b')
fit_0[which.min(fit_0[,2]), ]

##참고: 클러스터 개수 찾는 패키지
install.packages("cluster")
library(cluster)
install.packages("NbClust")
library(NbClust)

nc <- NbClust(iris[,1:4], min.nc=2, max.nc=6, method="kmeans")


#참고: Best k값 확인
nc
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 criteria")
