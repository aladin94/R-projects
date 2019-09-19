#Kernel PCA

data_set = read.csv('Social_Network_Ads.csv')
data_set = data_set[, 3:5]

library(caTools)
split = sample.split(data_set$Purchased, SplitRatio = 0.75)
training_set = subset(data_set, split == TRUE)
test_set = subset(data_set, split == FALSE)

training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

#Applying Kernel PCA
install.packages('kernlab')
library(kernlab)
kpca = kpca(~., data= training_set[-3], kernel = 'rbfdot', features = 2)
training_set_pca = as.data.frame(predict(kpca, training_set))
training_set_pca$Purchased = training_set$Purchased
test_set_pca = as.data.frame(predict(kpca, test_set))
test_set_pca$Purchased = test_set$Purchased

#Fitting logisitc regression on the Training set
classifer = glm(formula = Purchased ~ .,
                family = binomial,
                data = training_set_pca)

prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Confusion matrix
cm = table(test_set_pca[, 3], y_pred)
