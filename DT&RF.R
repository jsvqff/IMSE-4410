raw_data <- read.csv(file.choose(), header = T, sep = ",")
summary(raw_data)

raw_data$waterfront <- as.factor(raw_data$waterfront)
raw_data$condition <- as.factor(raw_data$condition)
raw_data$grade <- as.factor(raw_data$grade)
raw_data$zipcode <- as.factor(raw_data$zipcode)

raw_data$bedrooms[raw_data$bedrooms>16] <- NA
CleanData <- raw_data[complete.cases(raw_data),]
summary(CleanData)

set.seed(15)
data_splitting <- createDataPartition(CleanData$price, p = 0.75, list = F)
train <- CleanData[data_splitting, ]
test <- CleanData[-data_splitting, ]

ANNfit1 <- train(price ~., data = train, method = "nnet", trace = F, hidden = 10)
summary(ANNfit1)

ANNfit2 <- train(price ~., data = train, method = "nnet", trace = F, hidden = 5)
summary(ANNfit2)

ANNfit3 <- train(price ~., data = train, method = "nnet", trace = F, hidden = 2)
summary(ANNfit3)

RF_fit <- train(price ~., data = train, method = 'rf', ntree = 10)
RF_predict <- predict(RF_fit, newdata = test)
confusionMatrix(test$price, RF_predict, mode = "everything")
varImp(RF_fit)

library('Metrics')
mape(testing_data$rating, RF_predict)

rmse(testing_data$rating, RF_predict)

mae(testing_data$rating, RF_predict)

