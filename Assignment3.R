#Assignment 3

#q1)
library(ISLR)
set.seed(17) #set same as chapter 5
k = 10

#method
#get the remainder
#(total - remainder) % kvalue  = number of observation in each fold
#for each remainder observation, randomly insert into a fold
#loop through polynomial and then each fold. Nested for loop


num_obs = nrow(Auto)
remain_obs = num_obs%%10 #get modulo value
num_obs2 = num_obs-remain_obs # number divisible by k value

#sample dataset first before splitting into k
sampled_data = sample(num_obs, num_obs2) #returnsindex
#split sampled data into each fold
split_data = split(sampled_data, f = 1:k) #returns list of list
#get remainder datasets
remain_data = setdiff(1:num_obs, sampled_data) #131 and 161 left out

#randomly assign a k value to each of remainder. Add it to the kth fold
sample_kfold = sample(k, remain_obs)

#store cv results
cv.error=rep(0,k) #per poly you have 1 error. Take th mean across all fold

for (i in 1:remain_obs){
  split_data[[sample_kfold[i]]] = c(split_data[[sample_kfold[i]]] , sample_kfold[i])
}


for (poly in 1:10){
  mean_lst = numeric(10)
  for (kfold in 1:k){
    test_data = Auto[split_data[[kfold]], ] #split data 
    train_data = Auto[setdiff(1:num_obs, split_data[[kfold]]),]
    model=lm(mpg~poly(horsepower, degree=poly, raw=T), data=train_data) #train model
    pred=predict(model, test_data) #predict using fitted model
    
    #take sum of MSE, then divide by num to get average
    mean_lst[kfold] = sum((test_data$mpg-pred)^2)/length(split_data[[kfold]])
  }
  cv.error[poly] = mean(mean_lst)
}

#cv.error returns  24.38442 19.21583 19.23603 19.36616 18.99810 19.07437 18.98292 19.06402 19.06271 19.29920
CV.error = 24.23151 19.24821 19.33498 19.42443 19.03321


#Question 2

#2a)
set.seed(1)
X = rnorm(100)
noise = rnorm(100)
Y = 3 + 1*X + 4*X^2 - 1*X^3 + noise


#2b)
require(leaps)
df = data.frame(Y, X)
fit = regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10)

fit_summary =  summary(fit)

require(tidyverse)
require(ggplot2)
require(ggthemes);

data_frame(Cp = fit_summary$cp,
           BIC = fit_summary$bic,
           AdjR2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>% #add new
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_point() + geom_line() +  ylab('') + xlab('Number of Variables Selected') +
  facet_wrap(~ value_type, scales = 'free') +
  scale_x_continuous(breaks = 1:10)

coef(fit,which.min(fit_summary$bic))


#d)
require(caret)
#forward model
x_poly = poly(df$X, 10)

colnames(x_poly) <- paste0('poly', 1:10) #paste0 concatenates vectors after converting to character
model_for = train(y = Y, x = x_poly,
                   method = 'glmStepAIC', direction = 'forward', 
                   trace = 0,
                   trControl = trainControl(method = 'none', verboseIter = FALSE))

postResample(predict(model_for, data.frame(x_poly)), df$Y) #calculated mean squared error of 2 numeric vectors of data

summary(model_for$finalModel)


#backward
model_back = train(Y ~ poly(X, 10), data = df, 
                    method = 'glmStepAIC', direction = 'backward', 
                    trace = 0,
                    trControl = trainControl(method = 'none', verboseIter = FALSE))

postResample(predict(model_back, df), df$Y) #calculated mean squared error of 2 numeric vectors of data

summary(model_back$finalModel)