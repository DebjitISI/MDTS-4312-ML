rm(list=ls())
library(MASS)
attach(Boston)
small_data=Boston[,c('medv','nox','lstat','chas')]
#install.packages('tree')
library(tree)
?tree
tree.fit=tree(small_data$medv~.,small_data)
plot(tree.fit)
text(tree.fit,pretty = 0)
summary(tree.fit)
tree.fit
y_hat = predict(tree.fit,type='vector')
y_hat
train_mse = mean((y_hat-small_data$medv)^2)
lstat_new = mean(small_data$lstat)
chas_new = 1
nox_new = mean(small_data$nox)
df = data.frame(lstat= lstat_new,chas=chas_new,nox=nox_new)
predict(tree.fit,df)


tree.fit=tree(Boston$medv~.,Boston)
plot(tree.fit)
text(tree.fit,pretty = 0)
summary(tree.fit)
tree.fit
y_hat = predict(tree.fit,type='vector')
y_hat
train_mse = mean((y_hat-Boston$medv)^2)
train_mse


mult = lm(Boston$medv~.,Boston)
y_hat_mul = predict(mult)
train_se_mul = mean((y_hat_mul-Boston$medv)^2)
