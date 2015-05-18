df_train <- read.csv("G:/Kaggle_TFI/train.csv")
df_test <- read.csv("G:/Kaggle_TFI/test.csv")

df_sample <- df_train[inliers,]#[df_train$revenue < 3.93e6, ] #[sample(nrow(df_train), 13), ]
  
CG <- df_sample$City.Group
type <- df_sample$Type
P1 <- df_sample$P1
P2 <- df_sample$P2
P3 <- df_sample$P3
P4 <- df_sample$P4
P5 <- df_sample$P5
P6 <- df_sample$P6
P7 <- df_sample$P7
P8 <- df_sample$P8
P9 <- df_sample$P9
P10 <- df_sample$P10
P11 <- df_sample$P11
P12 <- df_sample$P12
P13 <- df_sample$P13
P14 <- df_sample$P14
P15 <- df_sample$P15
P16 <- df_sample$P16
P17 <- df_sample$P17
P18 <- df_sample$P18
P19 <- df_sample$P19
P20 <- df_sample$P20
P21 <- df_sample$P21
P22 <- df_sample$P22
P23 <- df_sample$P23
P24 <- df_sample$P24
P25 <- df_sample$P25
P26 <- df_sample$P26
P27 <- df_sample$P27
P28 <- df_sample$P28
P29 <- df_sample$P29
P30 <- df_sample$P30
P31 <- df_sample$P31
P32 <- df_sample$P32
P33 <- df_sample$P33
P34 <- df_sample$P34
P35 <- df_sample$P35
P36 <- df_sample$P36
P37 <- df_sample$P37
revenue <- df_sample$revenue / 1e6

fit1 <- glm(revenue ~ CG
            + P1
            + P2
            + P3
            + P4
            + P5
            + P6
            + P7
            + P8
            + P9
            + P10
            + P11
            + P12
            + P13
            + P14
            + P15
            + P16
            + P17
            + P18
            + P19
            + P20
            + P21
            + P22
            + P23
            + P24
            + P25
            + P26
            + P27
            + P28
            + P29
            + P30
            + P31
            + P32
            + P33
            + P34
            + P35
            + P36
            + P37
)
  
fit2 <- lm(revenue ~ P6 + P8 + 
             P17 + P20 + P28)


test_data <- data.frame(
  CG = df_test$City.Group,
  P1 = df_test$P1,
  P2 = df_test$P2,
  P3 = df_test$P3,
  P4 = df_test$P4,
  P5 = df_test$P5,
  P6 = df_test$P6,
  P7 = df_test$P7,
  P8 = df_test$P8,
  P9 = df_test$P9,
  P10 = df_test$P10,
  P11 = df_test$P11,
  P12 = df_test$P12,
  P13 = df_test$P13,
  P14 = df_test$P14,
  P15 = df_test$P15,
  P16 = df_test$P16,
  P17 = df_test$P17,
  P18 = df_test$P18,
  P19 = df_test$P19,
  P20 = df_test$P20,
  P21 = df_test$P21,
  P22 = df_test$P22,
  P23 = df_test$P23,
  P24 = df_test$P24,
  P25 = df_test$P25,
  P26 = df_test$P26,
  P27 = df_test$P27,
  P28 = df_test$P28,
  P29 = df_test$P29,
  P30 = df_test$P30,
  P31 = df_test$P31,
  P32 = df_test$P32,
  P33 = df_test$P33,
  P34 = df_test$P34,
  P35 = df_test$P35,
  P36 = df_test$P36,
  P37 = df_test$P37
)


predict_train <- predict(fit2)
predicted_data <- predict(fit2, test_data)
predicted_data <- predicted_data*1e6
submission <- data.frame(Id = df_test$Id, Prediction = predicted_data)
write.csv(submission, file="G:/Kaggle_TFI/submission.csv", row.names=FALSE, quote=FALSE)
plot(predict_train, df_sample$revenue/1e6)