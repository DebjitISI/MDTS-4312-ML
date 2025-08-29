rm(list=ls())
library(MASS)
attach(Boston)
small_data=Boston
library(tree)
set.seed(123)
index = sample(1:506,size=100,replace=F)

test_data = small_data[index,]
train_data = small_data[c(-index),]

tree_fit = tree(medv ~ .,data = train_data)

plot(tree_fit)
text(tree_fit,pretty=0)
summary(tree_fit)
optimal_tree = cv.tree(tree_fit,K = 8);optimal_tree
?cv.tree
plot(optimal_tree$size,optimal_tree$dev,type='o')

new_tree = prune.tree(tree_fit, best = 8)#best=9 is unpruned tree so we donnot use it
plot(new_tree)
text(new_tree,pretty = 0)

#Train MSE Optimal
y_hat_train_opt = predict(new_tree,type='vector',newdata = train_data)
train_mse_opt = mean((y_hat_train_opt - train_data$medv)^2)

#Test MSE Optimal
y_hat_test_opt = predict(new_tree,type='vector',newdata = test_data)
test_mse_opt = mean((y_hat_test_opt - test_data$medv)^2)

#Train MSE Unpruned
y_hat_train_old = predict(tree_fit,type='vector',newdata = train_data)
train_mse_old = mean((y_hat_train_old - train_data$medv)^2)

#Test MSE Unpruned
y_hat_test_old = predict(tree_fit,type='vector',newdata = test_data)
test_mse_old = mean((y_hat_test_old - test_data$medv)^2)

