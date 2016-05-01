rm(list=ls(all=TRUE))

library(xgboost)
library(Matrix)
library(AUC)

set.seed(12224)

train <- read.csv("input/train.csv")

features <- read.csv("features.csv")

features <- c(as.matrix(features[,2]),"TARGET")
features <- rev(features[1:153])
features <- features[93:153]
train <- train[,c(features,"TARGET")]

train.y <- train$TARGET[1:61000]
val.y <- train$TARGET[61001:76020]

results.numb <- integer()
results.seeds <- integer()
results.aucs <- double()

val <- train[61001:76020,]
train <- train[1:61000,]

val.y <- val$TARGET
val$TARGET <- NULL
#seeds <- c(1111,1112,1113,1114,1121,1122,1123,1124,1131,1132,1133,1134,1141,1142,1143,1144,1211,1212,1213,1214,1221,1222,1223,1224,1231,1232,1233,1234,1241,1242,1243,1244,1311,1312,1313,1314,1321,1322,1323,1324,1331,1332,1333,1334,1341,1342,1343,1344,1411,1412,1413,1414,1421,1422,1423,1424,1431,1432,1433,1434,1441,1442,1443,1444)
#seeds <- seeds +2000
seeds <- 3311
for (s in seeds){
  print(s)
  set.seed(s)
i <-0
for(f in features) {
  print(f)
  print(i)
  #train[,f]<-NULL
  #val[,f]<-NULL
  trainp <- sparse.model.matrix(TARGET ~ ., data = train)
  dtrain <- xgb.DMatrix(data=trainp, label=train.y)
  watchlist <- list(trainp=dtrain)
  i <- i+1
  if (i== 2){
    break
  }
  param <- list(  objective           = "binary:logistic", 
                  booster             = "gbtree",
                  eval_metric         = "auc",
                  eta                 = 0.0202048,
                  max_depth           = 5,
                  subsample           = 0.6815,
                  colsample_bytree    = 0.701
  )
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 560,
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
  results.numb <- c(results.numb,i)
  results.aucs <- c(results.aucs,auc(roc(preds,val.y)))
  results.seeds <- c(results.seeds,s)
  print(auc(roc(preds,val.y)))
      
}
}
#k <- matrix(c(results.aucs,results.numb,results.seeds),ncol = 3)
#p <- read.csv("explatory.csv")
#u <- rbind(p[,2:6],k)
#write.csv(u,"explatory.csv")
#submission <- data.frame(ID=test.id, TARGET=preds)
#cat("saving the submission file\n")
#write.csv(submission, "submission.csv", row.names = F)

test  <- read.csv("input/test.csv")
test.id <- test$ID
test$ID <- NULL
test <- test[,features]
test$TARGET <- NULL
test$TARGET <- -1
valmat <- sparse.model.matrix(TARGET ~ ., data = test) 
preds <- predict(clf, valmat)
submission <- data.frame(ID=test.id, TARGET=preds)
write.csv(submission, "submission.csv", row.names = F)