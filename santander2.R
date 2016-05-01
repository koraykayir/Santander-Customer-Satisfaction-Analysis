rm(list=ls(all=TRUE))

library(xgboost)
library(Matrix)
library(AUC)

set.seed(1234)

train <- read.csv("input/train.csv")
test  <- read.csv("input/test.csv")

train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

train.y <- train$TARGET
train$TARGET <- NULL

count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]

train$TARGET <- train.y


train <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)


param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.0188,
                max_depth           = 5,
                subsample           = 0.64,
                colsample_bytree    = 0.67
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 563, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

test$TARGET <- NULL
test$TARGET <- -1
valmat <- sparse.model.matrix(TARGET ~ ., data = test)      
preds <- predict(clf, valmat)


        
        

# k <- matrix(c(results.aucs,results.etas,results.subs,results.rounds,results.colsamps),ncol = 5)
submission <- data.frame(ID=test.id, TARGET=preds)
#cat("saving the submission file\n")
write.csv(submission, "submission.csv", row.names = F)