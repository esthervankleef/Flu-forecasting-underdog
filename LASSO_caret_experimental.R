require(RCurl)
require(prettyR)
library(e1071)
#from: https://www.r-bloggers.com/cross-validation-for-predictive-analytics-using-r/
  
url <- "https://raw.githubusercontent.com/gastonstat/CreditScoring/master/CleanCreditScoring.csv"
cs_data <- getURL(url)
cs_data <- read.csv(textConnection(cs_data))
describe(cs_data)


require(caret)

classes <- cs_data[, "Status"]
predictors <- cs_data[, -match(c("Status", "Seniority", "Time", "Age", "Expenses", 
                                 "Income", "Assets", "Debt", "Amount", "Price", "Finrat", "Savings"), colnames(cs_data))]

train_set <- createDataPartition(classes, p = 0.8, list = FALSE)
str(train_set)
train_predictors <- predictors[train_set, ]
train_classes <- classes[train_set]
test_predictors <- predictors[-train_set, ]
test_classes <- classes[-train_set]

set.seed(seed)
cv_splits <- createFolds(classes, k = 10, returnTrain = TRUE)
str(cv_splits)


require(glmnet)
## Warning: package 'Matrix' was built under R version 3.2.5
set.seed(seed)

cs_data_train <- cs_data[train_set, ]
cs_data_test <- cs_data[-train_set, ]

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(Status ~ ., data = cs_data_train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)
glmnet_fit

glmnet(y=Status, x=cs)
trellis.par.set(caretTheme())
plot(glmnet_fit, scales = list(x = list(log = 2)))

pred_classes <- predict(glmnet_fit, newdata = cs_data_test)
table(pred_classes)

pred_probs <- predict(glmnet_fit, newdata = cs_data_test, type = "prob")
head(pred_probs)
