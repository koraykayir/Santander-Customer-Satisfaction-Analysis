rm(list=ls(all=TRUE))

library(som)
library(matrixStats)
library(neuralnet)
library(xgboost)

train <- read.csv("input/train.csv")
test  <- read.csv("input/test.csv")

trainF <- train[,1:370]
trainL <- train[,371:371]

testF <- test[,1:370]

testF <- as.matrix(testF)
trainF <- as.matrix(trainF)

trainF <- trainF[,which(!apply(trainF,2,FUN = function(x){all(x == 0)}))]
testF <- testF[,which(!apply(trainF,2,FUN = function(x){all(x == 0)}))]

trainF[,"var36"] <- replace(trainF[,"var36"], trainF[,"var36"] == 99, 0)
testF[,"var36"] <- replace(testF[,"var36"], testF[,"var36"] == 99, 0)

trainL <- as.matrix(trainL)
atr<-matrix(c(trainF[,"ind_var5"],trainF[,"ind_var30"],trainF[,"num_meses_var5_ult3"],trainF[,"var36"],trainF[,"ind_var43_recib_ult1"],trainF[,"ind_var41_0"],trainF[,"ind_var39_0"],trainF[,"ind_var9_cte_ult1"],trainF[,"ind_var10cte_ult1"],trainL),ncol = 10)


#testL <- test[,371:371]


#testL <- as.matrix(testL)
ate<-matrix(c(testF[,"ind_var5"],testF[,"ind_var30"],testF[,"num_meses_var5_ult3"],testF[,"var36"],testF[,"ind_var43_recib_ult1"],testF[,"ind_var41_0"],testF[,"ind_var39_0"],testF[,"ind_var9_cte_ult1"],testF[,"ind_var10cte_ult1"]),ncol = 9)

normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}
ate[,4] <- replace(ate[,4], ate[,4] == 99, 0)
atr[,4] <- replace(atr[,4], atr[,4] == 99, 0)
atr<-apply(atr,2,normalit)
ate<-apply(ate,2,normalit)


trainDataF <- trainF[,2:336]

n <- colnames(trainF)
n <- n[2:336]
names(trainDataF) <- n

trainDataF <- apply(trainDataF,2,normalit)

vars.col <- colVars(trainDataF)
names(vars.col) <- n
vars.col <- as.matrix(vars.col/sum(vars.col))

normTr <- normalize(trainF[,2:336], byrow = F)
normTe <- normalize(testF[,2:336], byrow = F)

for (f in names(normTr)) {
  if (length(unique(normTr[[f]])) == 1) {
    normTr[[f]] <- NULL
    normTe[[f]] <- NULL
  }
}

#features_pair <- combn(names(normTr), 2, simplify = F)
#toRemove <- c()
#for(pair in features_pair) {
#  f1 <- pair[1]
#  f2 <- pair[2]
#  
#  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
#    if (all(normTr[[f1]] == normTr[[f2]])) {
#      toRemove <- c(toRemove, f2)
#    }
#  }
#}

#pca.train <- prcomp(normTr, center=T, scale=T)