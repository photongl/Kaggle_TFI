library(randomForest)
df_train <- read.csv("G:/Kaggle_TFI/train.csv")
df_test <- read.csv("G:/Kaggle_TFI/test.csv")


w <- rep(1, 37)
x_train <- data.matrix(df_train[, 6:42])
x_test <- data.matrix(df_test[, 6:42])

model.rf <- randomForest(df_train$revenue ~ .,
                        data=df_train[, 6:42],
                        mtry=5,
                        ntree=100,
                        strata = df_train$P8,
                        sampsize=68)
print(model.rf)

    
#obs[i] <- predict(model.rf, newdata=test_row)
predicted_data_rf <- predict(model.rf, newdata=df_test[, 6:42])



plot(x_train %*% w, y = df_train$revenue, col="red",pch=2)
points(x_test %*% w, predicted_data_rf, col="blue", pch=1)

submission <- data.frame(Id = df_test$Id, Prediction = predicted_data_rf)
write.csv(submission, file="G:/Kaggle_TFI/submission_rf.csv", row.names=FALSE, quote=FALSE)
