# class boundaries
b = c(200, 500, 750)


df_train <- read.csv("G:/Kaggle_TFI/train.csv")
df_mc <- data.frame(x=2*(df_train$P1 +
                           df_train$P2 +
                           df_train$P3 +
                           df_train$P4 +
                           df_train$P5 +
                           df_train$P6 +
                           df_train$P7 +
                           df_train$P8 +
                           df_train$P9 +
                           df_train$P10 +
                           df_train$P11 +
                           df_train$P12 +
                           df_train$P13 +
                           df_train$P14 +
                           df_train$P15 +
                           df_train$P16 +
                           df_train$P17 +
                           df_train$P18 +
                           df_train$P19 +
                           df_train$P20 +
                           df_train$P21 +
                           df_train$P22 +
                           df_train$P23 +
                           df_train$P24 +
                           df_train$P25 +
                           df_train$P26 +
                           df_train$P27 +
                           df_train$P28 +
                           df_train$P29 +
                           df_train$P30 +
                           df_train$P31 +
                           df_train$P32 +
                           df_train$P33 +
                           df_train$P34 +
                           df_train$P35 +
                           df_train$P36 +
                           df_train$P37
                         ), y=df_train$revenue)
df_test <- read.csv("G:/Kaggle_TFI/test.csv")
x_test <- 2*(  					df_test$P1 +
                      df_test$P2 +
                      df_test$P3 +
                      df_test$P4 +
                      df_test$P5 +
                      df_test$P6 +
                      df_test$P7 +
                      df_test$P8 +
                      df_test$P9 +
                      df_test$P10 +
                      df_test$P11 +
                      df_test$P12 +
                      df_test$P13 +
                      df_test$P14 +
                      df_test$P15 +
                      df_test$P16 +
                      df_test$P17 +
                      df_test$P18 +
                      df_test$P19 +
                      df_test$P20 +
                      df_test$P21 +
                      df_test$P22 +
                      df_test$P23 +
                      df_test$P24 +
                      df_test$P25 +
                      df_test$P26 +
                      df_test$P27 +
                      df_test$P28 +
                      df_test$P29 +
                      df_test$P30 +
                      df_test$P31 +
                      df_test$P32 +
                      df_test$P33 +
                      df_test$P34 +
                      df_test$P35 +
                      df_test$P36 +
                      df_test$P37)

x1 <- df_mc[df_mc$x < b[1], ]$x
y1 <- df_mc[df_mc$x < b[1], ]$y
model1 <- lm(y1 ~ x1)

x2 <- df_mc[df_mc$x >= b[1] & df_mc$x < b[2], ]$x
y2 <- df_mc[df_mc$x >= b[1] & df_mc$x < b[2], ]$y
model2 <- lm(y2 ~ x2)

x3 <- df_mc[df_mc$x >= b[3] & df_mc$x < b[4], ]$x
y3 <- df_mc[df_mc$x >= b[3] & df_mc$x < b[4], ]$y
model3 <- lm(y3 ~ x3)

N = length(x_test)
predicted_data <- numeric(N)

for (i in 1:N)
{
  if (x_test[i] < b[1])
  {
    predicted_data[i] = model1$coefficients[1] + model1$coefficients[2]*x_test[i]
  }
  if (x_test[i] >= b[1] & x_test[i] < b[2])
  {
    predicted_data[i] = model2$coefficients[1] + model2$coefficients[2]*x_test[i]
  }
  if (x_test[i] >= b[2] & x_test[i] < b[3])
  {
    # blended model
    t = (x_test[i] - b[2]) / (b[3] - b[2])
    predicted_data[i] = (1-t)*(model2$coefficients[1] + model2$coefficients[2]*x_test[i])+
                        t*(model3$coefficients[1] + model3$coefficients[2]*x_test[i])
  }
  if (x_test[i] >= b[3])
  {
    predicted_data[i] = model3$coefficients[1] + model3$coefficients[2]*x_test[i]
  }
}

submission <- data.frame(Id = df_test$Id, Prediction = predicted_data)
write.csv(submission, file="G:/Kaggle_TFI/submission_mc.csv", row.names=FALSE, quote=FALSE)
