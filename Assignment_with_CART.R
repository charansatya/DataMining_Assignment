setwd ("C:\\BABI\\Data Mining\\Asssignment\\DataSheets")
getwd()

## Data Import
hrData <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
hrData <- hrData[-c(9,10,22)]
colnames(hrData)
sapply(hrData, function(x) sum(is.na(x)))

prop.table(table(hrData$Attrition))

s <- sample(c(1:2940), size = 2058)
hrData.Train=hrData[s,]
hrData.Test=hrData[-s,]

prop.table(table(hrData.Train$Attrition))
prop.table(table(hrData.Test$Attrition))
table(hrData$Attrition)
library(rpart)
library(rpart.plot)
r.ctrl_1 = rpart.control(minsplit=147, minbucket = 50, cp = 0, xval = 10)
r.ctrl_2 = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)
hrModel_1 <- rpart(formula = Attrition ~ ., 
            data = hrData.Train, method = "class", 
            control = r.ctrl_1)
hrModel_2 <- rpart(formula = Attrition ~ ., 
                   data = hrData.Train, method = "class", 
                   control = r.ctrl_2)

install.packages("rattle")
install.packages("RColorBrewer")
library(rattle)

library(RColorBrewer)
fancyRpartPlot(hrModel_1)
fancyRpartPlot(hrModel_2)

## to find how the tree performs
printcp(hrModel_1) #cost complexity
plotcp(hrModel_1)

printcp(hrModel_2) #cost complexity
plotcp(hrModel_2)

ptree=prune(hrModel_1, cp= 0.032 ,"CP")
printcp(ptree)
fancyRpartPlot(ptree)

hrData.Train$predict.class <- predict(hrModel_1, hrData.Train, type="class")
hrData.Train$predict.score <- predict(hrModel_1, hrData.Train,type = "prob")

table(hrData.Train$predict.class,hrData.Train$Attrition)
## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

class(hrData.Train$predict.score)
## deciling
hrData.Train$deciles <- decile(hrData.Train$predict.score[,2])
View(hrData.Train)

remove.packages(c("ggplot2", "data.table"))
install.packages("ROCR",dep=T)
install.packages("ROCR")
trace(utils:::unpackPkgZip, edit=TRUE)
install.packages("gplots")
library(ROCR)
??ROCR
library("gdata")
library(gplots)
pred <- prediction(hrData.Train$predict.score[,2], hrData.Train$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini=2*auc-1

with(hrData.Train, table(Attrition, predict.class))
auc
KS
gini


#Testing data

hrData.Test$predict.class <- predict(hrModel_1, hrData.Test, type="class")
hrData.Test$predict.score <- predict(hrModel_1, hrData.Test)

pred_test <- prediction(hrData.Test$predict.score[,2], hrData.Test$Attrition)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test)
KS_test <- max(attr(perf_test, 'y.values')[[1]]-attr(perf_test, 'x.values')[[1]])
auc_test <- performance(pred_test,"auc"); 
auc_test <- as.numeric(auc_test@y.values)
gini_test=2*auc_test-1

with(hrData.Test, table(Attrition, predict.class))
auc_test
KS_test
gini_test


