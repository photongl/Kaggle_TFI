library(glmnet)
df_train <- read.csv("G:/Kaggle_TFI/train.csv")
df_test <- read.csv("G:/Kaggle_TFI/test.csv")


x_train <- data.matrix(df_train[, 6:42])
model.reg <- glmnet(x = x_train, y = df_train$revenue[inliers])
x_test <- data.matrix(df_test[, 6:42])
predicted_data <- predict(model.reg, newx = x_test, s=c(350400))

layout(matrix(1:2),2,1)
hist(df_train$revenue, breaks=20, xlim=c(1e5, 2.5e7))
hist(predicted_data, breaks=20, xlim=c(1e5, 2.5e7))

submission <- data.frame(Id = df_test$Id, Prediction = predicted_data)
write.csv(submission, file="G:/Kaggle_TFI/submission_reg.csv", row.names=FALSE, quote=FALSE)