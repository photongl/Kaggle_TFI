df_train <- read.csv("G:/Kaggle_TFI/train.csv")
df_test <- read.csv("G:/Kaggle_TFI/test.csv")

w <- rep(1, 37)#runif(37)
x <- c(data.matrix(df_train[, 6:42]) %*% w)

fit <- lm(log(df_train$revenue) ~ poly(x,6))


x_test <- data.frame(x=data.matrix(df_test[, 6:42]) %*% w)
predicted_data_poly <- exp(predict(fit, x_test))


plot(x, df_train$revenue, pch=2, col="red")
points(x_test$x, predicted_data_poly, col="blue", pch=1)

submission <- data.frame(Id = df_test$Id, Prediction = predicted_data_poly)
write.csv(submission, file="G:/Kaggle_TFI/submission_poly.csv", row.names=FALSE, quote=FALSE)