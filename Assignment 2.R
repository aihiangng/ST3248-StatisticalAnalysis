#Assignment 2
#1a
lm.fit=lm(Sales~Price+Urban+US ,data=Carseats)
summary(lm.fit)

#1d
lm.fit2=lm(Sales~Price+US ,data=Carseats)
summary(lm.fit2)

#1f
confint(lm.fit2)

#1g
par(mfrow = c(2, 2))
plot(lm.fit2)


plot(hatvalues(lm.fit2))



#q2
library(MASS)
attach(Boston)
#detach(Boston)
names(Boston)
pairs(Boston)

#create col for outcome
crime01 = ifelse(crim>median(crim), 1, 0)

Boston = data.frame(Boston,crime01)
Boston #506 datapoints


# Split train test set
set.seed(10)
train=sample(506,406) #leave 100 for test
train.X = data.frame(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[train,]
test.X = data.frame(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[-train,]
train.Y = crime01[train]
test.Y = crime01[-train]

#Logistic Regression
#(f)
glm.fit=glm(train.Y~. ,data=train.X,family="binomial")
glm.probs=predict(glm.fit, test.X, type="response")
glm.class=ifelse(glm.probs>0.5,1,0)
summary(glm.fit)
table(glm.class,test.Y)
1-mean(glm.class==test.Y)


train.X2 = data.frame( nox, rad, tax, ptratio, black,  medv)[train,]
test.X2 = data.frame( nox,rad, tax, ptratio, black,  medv)[-train,]
train.Y2 = crime01[train]
test.Y2 = crime01[-train]
glm.fit2=glm(train.Y2~. ,data=train.X2,family="binomial")
glm.probs2=predict(glm.fit2, test.X2, type="response")
glm.class2=ifelse(glm.probs2>0.5,1,0)
summary(glm.fit2)
table(glm.class2,test.Y2)
1-mean(glm.class2==test.Y2)

#LDA
lda.fit=lda(train.Y~. ,data=train.X)
lda.pred=predict(lda.fit, test.X)
lda.class=lda.pred$class
table(lda.class,test.Y)
1-mean(lda.class==test.Y) #test error

lda.fit2=lda(train.Y2~. ,data=train.X2)
lda.pred2=predict(lda.fit2, test.X2)
lda.class2=lda.pred2$class
table(lda.class2,test.Y2)
1-mean(lda.class2==test.Y2) #test error

#QDA
qda.fit=qda(train.Y~. ,data=train.X)
qda.pred=predict(qda.fit, test.X)
qda.class=qda.pred$class
table(qda.class,test.Y)
1-mean(qda.class==test.Y) #test error

train.X3 = data.frame(zn, indus, rm, age, dis, rad, ptratio, black, lstat, medv)[train,]
test.X3 = data.frame(zn, indus, rm, age, dis, rad, ptratio, black, lstat, medv)[-train,]
train.Y3 = crime01[train]
test.Y3 = crime01[-train]

qda.fit2=qda(train.Y3~. ,data=train.X3)
qda.pred2=predict(qda.fit2, test.X3)
qda.class2=qda.pred2$class
table(qda.class2,test.Y3)
1-mean(qda.class2==test.Y3) #test error

#KNN
library(class)
knn.pred=knn(train.X,test.X,train.Y,k=1)
conmat=table(knn.pred,test.Y)
conmat
1-sum(diag(conmat))/sum(conmat)


knn.pred2=knn(train.X,test.X,train.Y,k=3)
conmat2=table(knn.pred2,test.Y)
conmat2
1-sum(diag(conmat2))/sum(conmat2)

knn.pred3=knn(train.X,test.X,train.Y,k=10)
conmat3=table(knn.pred3,test.Y)
conmat3
1-sum(diag(conmat3))/sum(conmat3)

knn.errors = numeric(100)
for(i in 1:100){
  knn.pred=knn(train.X,test.X,train.Y,k=i)
  conmat=table(knn.pred,test.Y)
  knn.errors[i]=1-sum(diag(conmat))/sum(conmat)
}
plot(knn.errors)

