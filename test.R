library(ISLR2)
library(dplyr)
library(glmnet)
library(caret)
set.seed(0)

alpha = 1
k = 10

Cars_raw = Carseats
response_var <- "Sales"
lambda = 10^seq(10, -2, length = 100)
#------------------------
# change all qualitative vars into qualitative also omit rows with any missing
# var values

cars_quant = mydata_quant <- na.omit(Cars_raw) %>%
  mutate_all(~ if(is.factor(.)) {
    as.numeric(as.factor(.))
  } else {
    .
  })

col_num <- which(colnames(cars_quant) == response_var)
#--------------------------------------------------------------
# Divide the data into training and test

x = model.matrix(Sales ~ ., data=cars_quant)[, -1]
y = cars_quant$Sales

train = sample(1:nrow(x), 0.7*nrow(x)) # 70% training data
test = (-train)
y.test <- y[test]

#----------------------------------------------------------------
# Fit on training data
shrink.mod <- glmnet(x[train,], y[train], alpha = alpha, lambda = lambda, family="gaussian")

#------------------------------------------------
# preform cross validation to get the best lambda

cv.out = cv.glmnet(x[train,], y[train], alpha=alpha, nfolds=k, family="gaussian")
bestlam = cv.out$

#----------------------------------------------
# Generalization Error
shrink.pred <- predict(shrink.mod , s = bestlam , newx = x[test, ], family="gaussian")

shrink.pred = ifelse(shrink.pred > 5, 1, 0)
y.test = ifelse(y.test > 5, 1, 0)


TP = nrow(subset(y.test, y.test == 1))
TN = nrow(subset(y.test, y.test == 0))

FP = nrow(subset(shrink.pred, shrink.pred == 1))
FN = nrow(subset(shrink.pred, shrink.pred == 0))

accuracy = (TP + TN)/(TP + TN + FP + FN)
print("Accuracy")
print(accuracy)


plot(
  x = y.test, y = abs(y.test - shrink.pred),
  xlab = "Observed Values",
  ylab = "abs(Actual - Predicted)",
  main = "Observed Values vs. abs(Actual - Predicted)"
)
#----------------------------------------------
# looking at Coefficients

print("Coefficients")

shrink.coef = predict(shrink.mod,  type = "coefficients", s = bestlam)
print(shrink.coef)