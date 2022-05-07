rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots

getwd()
# setwd() set your work directory

library(tidyverse)
library(caret)
library(randomForest)
library(mgcv)
library(xgboost)
library(tree)
library(MLmetrics)
library(lubridate)
library(geosphere)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)

# load the dataset
taxi = read.csv('train.csv')
metars = read.csv('KNYC_Metars.csv')
district = read.csv('combined neighborhood and boro info.csv')
zipcode = read.csv("train_with_zipcode.csv")

taxi$date = format(as.Date(taxi$pickup_datetime), "%d-%m-%Y")
metars$date = format(as.Date(metars$Time), "%d-%m-%Y")

df = metars %>% 
  group_by(date) %>%
  summarise_at(vars(Visibility), list(name = mean))

colnames(df)[2]= "avg.visibility"

taxi.join = left_join(taxi, df[,c("date", "avg.visibility")])

taxi.join = left_join(taxi.join, district[,c("id", "pickup_boro", "pickup_boro_code",
                                             "pickup_neighborhood_name", "pickup_neighborhood_code",
                                             "dropoff_boro", 'dropoff_boro_code',
                                             "dropoff_neighborhood_name", "dropoff_neighborhood_name")])

taxi.join = left_join(taxi.join, zipcode[,c("id", "zipcode_from", "zipcode_to")])

pick_coord <- taxi.join %>%
  select(pickup_longitude, pickup_latitude)

drop_coord <- taxi.join %>%
  select(dropoff_longitude, dropoff_latitude)

taxi.join$dist <- distVincentyEllipsoid(pick_coord, drop_coord)

taxi.join$bearing = bearing(pick_coord, drop_coord)

taxi.join$hour = factor(hour(taxi.join$pickup_datetime))

taxi.join$wday = factor(wday(taxi.join$pickup_datetime))

taxi.join$trip_duration = taxi.join$trip_duration/60

# remove outlier in trip duration, distance
qn = quantile(taxi.join$trip_duration, c(0.05, 0.95), na.rm = TRUE)
taxi.join = within(taxi.join, { trip_duration = ifelse(trip_duration < qn[1], qn[1], trip_duration)
trip_duration = ifelse(trip_duration > qn[2], qn[2], trip_duration)})

qn = quantile(taxi.join$dist, c(0.05, 0.95), na.rm = TRUE)
taxi.join = within(taxi.join, { dist = ifelse(dist < qn[1], qn[1], dist)
dist = ifelse(dist > qn[2], qn[2], dist)})

# Remove number of passengers > 6
taxi.join = taxi.join[taxi.join$passenger_count <= 6,]

# filter 0 value
taxi.join = taxi.join[taxi.join$passenger_count >0,]

# drop na
taxi.join <- na.omit(taxi.join)
sum(is.na(taxi.join))


# train-test split
set.seed(100)
index = createDataPartition(y = taxi.join$trip_duration, p = 0.8, list = F)
train = taxi.join[index, ]
test = taxi.join[-index, ]

myCoreNums <- detectCores()
cl = makeCluster(myCoreNums-1)

# linear regression
registerDoParallel(cl) # parallelize to speed up

lm = train(trip_duration~dist+wday+hour+passenger_count+bearing+avg.visibility, 
           data = train, method = "lm")

pred_lm = predict(lm, newdata = test)
summary(lm) # review the performance on training set

lm_mape = round(MAPE(pred_lm, test$trip_duration), digits = 4)
lm_mae = round(MAE(pred_lm, test$trip_duration), digits = 4)
lm_rmse = round(RMSE(pred_lm, test$trip_duration), digits = 4)
lm_r2 = round(R2(pred_lm, test$trip_duration), digits = 4)

lm_mape
lm_mae
lm_rmse
lm_r2

stopCluster(cl)

# decision tree
registerDoParallel(cl) # parallelize to speed up

tree = tree(trip_duration~dist+wday+hour+passenger_count+bearing+avg.visibility, data = train)
pred_tree = predict(tree, newdata = test)
tree
summary(tree)

tree_mape = round(MAPE(pred_tree, test$trip_duration), digits = 4)
tree_mae = round(MAE(pred_tree, test$trip_duration), digits = 4)
tree_rmse = round(RMSE(pred_tree, test$trip_duration), digits = 4)
tree_r2 = round(R2(pred_tree, test$trip_duration), digits = 4)

tree_mape
tree_mae
tree_rmse
tree_r2

stopCluster(cl)

# gam
registerDoParallel(cl) # parallelize to speed up

gam = gam(trip_duration~s(dist)+wday+hour+passenger_count+s(bearing)+
            s(avg.visibility), data = train)
pred_gam = predict(gam, newdata = test)
summary(gam) # review the performance on training set

gam_mape = round(MAPE(pred_gam, test$trip_duration), digits = 4)
gam_mae = round(MAE(pred_gam, test$trip_duration), digits = 4)
gam_rmse = round(RMSE(pred_gam, test$trip_duration), digits = 4)
gam_r2 = round(R2(pred_gam, test$trip_duration), digits = 4)

gam_mape
gam_mae
gam_rmse
gam_r2

stopCluster(cl)

# random forest
registerDoParallel(cl) # parallelize to speed up

set.seed(100)
rf = randomForest(trip_duration~dist+wday+hour+passenger_count+bearing+
                    avg.visibility, data = train,
                  ntree = 100, sampsize = 10000, importance=TRUE)
pred_rf = predict(rf, newdata = test)
rf # review the performance on training set
importance(rf)

rf_mape = round(MAPE(pred_rf, test$trip_duration), digits = 4)
rf_mae = round(MAE(pred_rf, test$trip_duration), digits = 4)
rf_rmse = round(RMSE(pred_rf, test$trip_duration), digits = 4)
rf_r2 = round(R2(pred_rf, test$trip_duration), digits = 4)

rf_mape
rf_mae
rf_rmse
rf_r2

stopCluster(cl)

# xgboost
registerDoParallel(cl) # parallelize to speed up

x_train = data.matrix(train[, c("dist", "wday", "hour", "passenger_count","bearing",
                                "avg.visibility")]) 
y_train = train[, c("trip_duration")]

x_test = data.matrix(test[, c("dist", "wday", "hour", "passenger_count","bearing",
                              "avg.visibility")])                   
y_test = test[, c("trip_duration")]

xgboost_train = xgb.DMatrix(data=x_train, label=y_train)
xgboost_test = xgb.DMatrix(data=x_test, label=y_test)

xgboost <- xgboost(data = xgboost_train, max_depth=7, nrounds=100)

pred_xgboost = predict(xgboost, xgboost_test)

xgboost # review the performance on training set
xgb.importance(colnames(xgboost_train), model = xgboost)

xgboost_mape = round(MAPE(pred_xgboost, test$trip_duration), digits = 4)
xgboost_mae = round(MAE(pred_xgboost, test$trip_duration), digits = 4)
xgboost_rmse = round(RMSE(pred_xgboost, test$trip_duration), digits = 4)
xgboost_r2 = round(R2(pred_xgboost, test$trip_duration), digits = 4)

xgboost_mape
xgboost_mae
xgboost_rmse
xgboost_r2

stopCluster(cl)

# compare model performance
model_result = data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest", "Generalized Additive Model", "XGBoost"),
  MAPE = c(lm_mape, tree_mape, rf_mape, gam_mape, xgboost_mape),
  MAE = c(lm_mae, tree_mae, rf_mae, gam_mae, xgboost_mae),
  RMSE = c(lm_rmse, tree_rmse, rf_rmse, gam_rmse, xgboost_rmse),
  R2 = c(lm_r2, tree_r2, rf_r2, gam_r2, xgboost_r2))

model_result

# myCoreNums <- detectCores()
# cl = makeCluster(myCoreNums-1)
# registerDoParallel(cl) # parallelize to speed up
# write.csv(taxi.join,"taxi_join.csv", row.names = FALSE)
# stopCluster(cl)

