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

val <- train[61001:76020,]
train <- train[1:61000,]
val$ID <- NULL

val.y <- val$TARGET
val$TARGET <- NULL
train.y <- train$TARGET
train$TARGET <- NULL

count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)
val$n0 <- apply(val,1,FUN=count0)

for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    train[[f]] <- NULL
    test[[f]] <- NULL
    val[[f]] <- NULL
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
val <- val[,feature.names]
test <- test[, feature.names]

train$TARGET <- train.y


train <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)

results.etas <- c()
results.subs <- c()
results.colsamps <- c()
results.aucs <- c()
results.rounds <- c()

etas <- c(0.0186,0.0187,0.0188)
subsamples <- c(0.63,0.64,0.65)
colsubsamples <- c(0.65,0.66,0.67)
rounds <- c(560,563,565)
i <- 0
results <- data.frame(e=double(),s=double(),c=double(),r=integer(),auc=double())
for(e in etas) {
  for(s in subsamples) {
    for(c in colsubsamples) {
      for(r in rounds) {
        i <- i+1
        param <- list(  objective           = "binary:logistic", 
                        booster             = "gbtree",
                        eval_metric         = "auc",
                        eta                 = e,
                        max_depth           = 5,
                        subsample           = s,
                        colsample_bytree    = c
        )
        
        clf <- xgb.train(   params              = param, 
                            data                = dtrain, 
                            nrounds             = r, 
                            verbose             = 1,
                            watchlist           = watchlist,
                            maximize            = FALSE
        )
        #test$TARGET <- NULL
        #test$TARGET <- -1
        #valmat <- sparse.model.matrix(TARGET ~ ., data = test)
        
        #preds <- predict(clf, valmat)
        
        val$TARGET <- NULL
        val$TARGET <- -1
        valmat <- sparse.model.matrix(TARGET ~ ., data = val)
        
        preds <- predict(clf, valmat)
        
        val.y <- factor(val.y)
        results.etas <- c(results.etas,e)
        results.subs <- c(results.subs,s)
        results.colsamps <- c(results.colsamps,c)
        results.rounds <- c(results.rounds,r)
        results.aucs <- c(results.aucs,auc(roc(preds,val.y)))
        
      }
    }
  }
}
k <- matrix(c(results.aucs,results.etas,results.subs,results.rounds,results.colsamps),ncol = 5)
p <- read.csv("explatory.csv")
u <- rbind(p[,2:6],k)
write.csv(u,"explatory.csv")
#submission <- data.frame(ID=test.id, TARGET=preds)
#cat("saving the submission file\n")
#write.csv(submission, "submission.csv", row.names = F)