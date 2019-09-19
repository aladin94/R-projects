dataset = read.csv('Wine.csv')

library (caTools)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])


#PCA
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)

pca = preProcess(x = training_set[-14], method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)

#We are switching the placement of the columns. 'C' represents Vectors here:
training_set = training_set[c(2,3,1)]

test_set = predict(pca, test_set)
test_set = test_set[c(2,3,1)]

#We are building an SVM classifier for our ML model:
classifier = svm(formula = Customer_Segment ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

#Predictions based on the Test set
y_pred = predict(classifier, newdata = test_set[-3])

#Make a confusion matrix!
cm = table(test_set[, 3], y_pred)

#Time to Visualize our results!:
install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, 3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato', 'blue'))
points(set, pch = '21', bg = ifelse(set[, 3] == 1, 'green4', 'red3', 'blue2'))        