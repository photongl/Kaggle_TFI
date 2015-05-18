library(foreach)

# load data
df_train <- read.csv("G:/Kaggle_TFI/train.csv")
df_test <- read.csv("G:/Kaggle_TFI/test.csv")

# index of start and stop variables
vars <- 5+c(6, 8, 17, 20, 26, 28)
start = 6
end = 42

# convert data frame to matrix
x_train <- data.matrix(df_train[, vars])
x_test <- data.matrix(df_test[, vars])

# bounds
test_size <- nrow(x_test)
train_size <- nrow(x_train)
train_revenue <- df_train$revenue
# array of min-distances of test-points from train-points
predicted_data <- numeric(test_size)


# determine distances between the training points and the test points
dist_weights <- numeric(train_size)
lambda = 0.05
foreach(i = 1:test_size) %do%
{
  dist_weights <- exp(-lambda * rowSums((x_train - x_test[i,])*(x_train - x_test[i,])))  
  predicted_data[i] = sum(train_revenue * dist_weights) / sum(dist_weights)
}

submission <- data.frame(Id = df_test$Id, Prediction = predicted_data)
write.csv(submission, file="G:/Kaggle_TFI/submission_nn.csv", row.names=FALSE, quote=FALSE)