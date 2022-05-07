rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots

getwd()
# setwd() set your work directory

library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)

# Data Preparation
data = read_csv('hotel_bookings.csv')
glimpse(data)

# Check missing values
sum(is.na(data))
colSums(is.na(data))

# Replace missing values of children with 0
data$children[is.na(data$children)] <- 0
sum(is.na(data))

#Find numeric variables
numeric_hotel <-which(sapply(data,is.numeric))
length(numeric_hotel)

#correlations:find the most relevant numerical variables in order to train model
hotel_numVar <- data[, numeric_hotel]
cor_hotel_numVar <- cor(hotel_numVar)
cor_sorted <- as.matrix(sort(cor_hotel_numVar[,'is_canceled'],decreasing = TRUE))
cor_sorted

#Choose relevant numerical variables
model_data <- data[c(1,2,3,5,8,9,10,11,12,13,15,16,17,18,19,20,
                     23,27,28,29,30)]  
colnames(model_data)                 
str(model_data)

# Train-Test Split
set.seed(1)
index = createDataPartition(y = model_data$is_canceled, p = 0.8, list = F)
train = model_data[index, ]
test = model_data[-index, ]
target_test = factor(test$is_canceled)

# positive class = 1

# decision tree
tree =  train(factor(is_canceled) ~., data = train, method = "rpart")
tree
varImp(tree)$importance

pred_tree = predict(tree, newdata = test)
tree_cm = confusionMatrix(factor(pred_tree), factor(target_test), positive="1")
tree_cm

tree_cm_overall = tree_cm$overall
tree_accuracy = round(tree_cm_overall['Accuracy'],4)
tree_cm_byclass = tree_cm$byClass
tree_f1 = round(tree_cm$byClass['F1'],4)
tree_precision = round(tree_cm$byClass['Precision'],4)
tree_recall = round(tree_cm_byclass['Recall'],4)

tree_accuracy
tree_f1
tree_precision
tree_recall

# random forest
rf = randomForest(factor(is_canceled) ~., data = train,
                  ntree = 100,importance=TRUE)

rf
importance(rf, type=1)

pred_rf = predict(rf, newdata = test)
rf_cm = confusionMatrix(factor(pred_rf), factor(target_test), positive = "1")
rf_cm

rf_cm_overall = rf_cm$overall
rf_accuracy = round(rf_cm_overall['Accuracy'],4)
rf_cm_byclass = rf_cm$byClass
rf_f1 = round(rf_cm$byClass['F1'],4)
rf_precision = round(rf_cm$byClass['Precision'],4)
rf_recall = round(rf_cm_byclass['Recall'],4)

rf_accuracy
rf_f1
rf_precision
rf_recall

# xgboost
x_train = data.matrix(train[,-2])
x_test = data.matrix(test[,-2])                   

xgboost_train = xgb.DMatrix(data=x_train, label=train$is_canceled)
xgboost_test = xgb.DMatrix(data=x_test, label=test$is_canceled)

# train_control = trainControl(method = "cv", number = 5, search = "grid")
# 
# gbmGrid <-  expand.grid(max_depth = c(5:20), 
#                         nrounds = 50,    # number of trees
#                         # default values below
#                         eta = 0.3,
#                         gamma = 0,
#                         subsample = 1,
#                         min_child_weight = 1,
#                         colsample_bytree = 0.6)
# 
# xgboost.tune = train(factor(is_canceled) ~., data = train, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)
# print(xgboost.tune)

xgboost = xgboost(data = xgboost_train, max_depth=20, nrounds=100)
xgb.importance(colnames(xgboost_train), model = xgboost)

pred_xgboost = predict(xgboost, xgboost_test)
pred_xgboost =  as.numeric(pred_xgboost > 0.5)
xgboost_cm = confusionMatrix(factor(pred_xgboost), factor(target_test), 
                             positive = "1")
xgboost_cm

xgboost_cm_overall = xgboost_cm$overall
xgboost_accuracy = round(xgboost_cm_overall['Accuracy'],4)
xgboost_cm_byclass = xgboost_cm$byClass
xgboost_f1 = round(xgboost_cm$byClass['F1'],4)
xgboost_precision = round(xgboost_cm$byClass['Precision'],4)
xgboost_recall = round(xgboost_cm_byclass['Recall'],4)

xgboost_accuracy
xgboost_f1
xgboost_precision
xgboost_recall

# compare model performance
model_result = data.frame(
  Model = c("Decision Tree", "Random Forest", "XGBoost"),
  Accuracy = c(tree_accuracy, rf_accuracy, xgboost_accuracy),
  F1_Score = c(tree_f1, rf_f1, xgboost_f1),
  Precision = c(tree_precision, rf_precision, xgboost_precision),
  Recall = c(tree_recall, rf_recall, xgboost_recall))

model_result
