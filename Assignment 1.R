#a
college<-read.csv(file='/Users/aihiangng/Desktop/ST3248 Statistical Learning 1/Assignments/College.csv')

#b
rownames(college)=college[,1] #introduce a new column representing row names, placed at column 1
college=college[,-1] #remove the old previous col1, now we can differentiate between schools and other variables

#c1
summary(college)
#c2
pairs(college)
pairs(college[,c(1:10)])
#c3
college$Private<- as.factor(college$Private)
plot(college$Private, college$Outstate, xlab= "Private", ylab="Outstate")

#c4
Elite=rep("No",nrow(college)) #rep: replicate elements of vectors and lists, set evyth too no first
Elite[college$Top10perc >50]="Yes" #update yes/no in Elite
Elite=as.factor(Elite)
college=data.frame(college,Elite) #add column to college

summary(college)
plot(college$Elite, college$Outstate, xlab= "Elite", ylab="Outstate")


#exploring data

plot(college$Elite, college$perc.alumni, xlab= "Elite", ylab="Perc of Alumni Who Donate")
hist(college$Grad.Rate) #seems like a normal distribution
hist(college$PhD) #negatively skewed
plot(college$Accept, college$Apps, xlab= "Accept", ylab="Apps") #plot applications received against applications accepted
subset(college, college$Accept > 25000) #identify extreme case of high apps and high accept
hist(college$Personal) #most people spend between $500 to $1000
plot(college$Elite, college$Personal, xlab= "Elite", ylab="Personal") #median approximately the same



#TUTORIAL 2

#a)
set.seed(1)
x<-rnorm(100, mean = 0, sd = 1) #given 100 observations N(0,1)
eps<-rnorm(100, mean = 0, sd = 0.5) 
y = -1 + 0.5*x  + eps
length(y) #returns 100

#d)
plot(x,y)
#e)
lm.fit <- lm(y~x)
summary(lm.fit)

#f)
plot(y~x)
abline(lm.fit, col="red")
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

#g)
#less noise, decrease variance
set.seed(1)
x2<-rnorm(100, mean = 0, sd = 1)
eps2<-rnorm(100, mean = 0, sd = 0.1) 
y2 = -1 + 0.5*x2  + eps2
lm.fit2<-lm(y2~x2)
summary(lm.fit2)

plot(y2~x2); abline(lm.fit2, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")


#h)
#more noise, decrease variance
set.seed(1)
x3<-rnorm(100, mean = 0, sd = 1)
eps3<-rnorm(100, mean = 0, sd = 0.8) 
y3 = -1 + 0.5*x3  + eps3
lm.fit3<-lm(y3~x3)
summary(lm.fit3)

plot(y3~x3); abline(lm.fit3, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

#i)
confint(lm.fit);confint(lm.fit2); confint(lm.fit3)


