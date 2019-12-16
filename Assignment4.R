#Tutorial 8 Question 3
#We wish to predict per capita crime rate in the Boston data set.
#Try out some of the regression methods explored in Ch06. Present and discuss the results. Propose a final model and justify your answer.

library(MASS)
?Boston
data("Boston")
x=model.matrix(crim~.,Boston)[,-1]
y=Boston$crim
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]


#MODEL 1 - Ridge Regression
cv.out=cv.glmnet(x[train,],y[train],alpha=0,nfolds=10)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam #returns 05919159
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2) #returns 40.92395
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)

#MODEL 2 - LASSO
library(plotmo)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot_glmnet(lasso.mod)                             # default colors
plot_glmnet(lasso.mod, label=5) 

#plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam #returns 0.0514183
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) #returns 40.99066
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]

#MODEL 3 PCR
set.seed(1)
pcr.fit=pcr(crim~., data=Boston,scale=TRUE,subset=train,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=13)
mean((pcr.pred-y.test)^2)

#MODEL 4 PLS
set.seed(1)
pls.fit=plsr(crim~., data=Boston,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=6)
mean((pls.pred-y.test)^2)


#Tutorial 9 Qn 4
data("USArrests")
states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pr.out=prcomp(USArrests, scale=TRUE)

#first part similar to lab notes.
states = row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pr.out = prcomp(USArrests, scale=T)
names(pr.out)

#Means of variables that were used for scaling prior to implementing PCA
pr.out$center

#SDs of variables that were used for scaling prior to implementing PCA
pr.out$scale

#Column of matric gives corresponding principal component loading vector
pr.out$rotation

#Use formula to calculate PVE 
data = stdize(as.matrix(USArrests))
pca.res = prcomp(x=data)
pc.coef = pca.res$rotation
PVE1 = sum((data%*%pc.coef[,1])^2)/sum(as.matrix(data)^2)
PVE1 #returns 0.6200604
PVE2 = sum((data%*%pc.coef[,2])^2)/sum(as.matrix(data)^2)
PVE2 #returns 0.2474413
PVE3 = sum((data%*%pc.coef[,3])^2)/sum(as.matrix(data)^2)
PVE3 #returns 0.0891408
PVE4 = sum((data%*%pc.coef[,4])^2)/sum(as.matrix(data)^2)
PVE4 #returns 0.04335752

#Contains sd of each Principal Component
pr.out$sdev

#Obtain PVE of each component by squaring them
pr.var = pr.out$sdev^2
pr.var

#To compute PVE, divide each variance by total variance
pve = pr.var/sum(pr.var)
pve
#returns 0.62006039 0.24744129 0.08914080 0.04335752
