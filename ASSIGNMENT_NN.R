#WIth NN

setwd ("C:\\BABI\\Data Mining\\Asssignment\\DataSheets")
getwd()

## Data Import
hrData_NN <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
hrData_NN <- hrData_NN[-c(9,10,22,27)]

hrdata_subset=hrData_NN[-c(2)]
hrData_subset1=sapply(hrdata_subset,unclass)
hrData_subset1_scaled=scale(hrData_subset1)
hrData_NN_scaled=cbind(hrData_subset1_scaled,hrData_NN[2])

#install.packages("plyr")
library(plyr)
hrData_NN_scaled$Attrition <- revalue(hrData_NN_scaled$Attrition, c("Yes"=1))
hrData_NN_scaled$Attrition <- revalue(hrData_NN_scaled$Attrition, c("No"=0))

str(hrData_NN_scaled)

#####Business Travel
#Non-travel -0
#Travel_Frequently-1
#Travel_Rarely -2

s <- sample(c(1:2940), size = 2058)
hrData.Train=hrData_NN_scaled[s,]

hrData.Test=hrData_NN_scaled[-s,]

library(neuralnet)


hrData.Train$Attrition=as.numeric(as.character(hrData.Train$Attrition))
set.seed(1000)
nn1 <- neuralnet(formula = Attrition ~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager , 
                 data = hrData.Train, 
                 hidden = c(5,2),
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.01,
                 stepmax = 2000
                 ##startweights = startweightsObj
)

plot(nn1)

hrData.Train$Prob=nn1$net.result[[1]]

hist(hrData.Train$Prob)
sum((hrData.Train$Attrition - hrData.Train$Prob)^2)  /  2

library(ROCR)
pred <- ROCR::prediction(hrData.Train$Prob,hrData.Train$Attrition)
perf <- performance(pred, "tpr", "fpr")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = 2 * auc - 1
auc
KS
gini


hrData.Train$Class = ifelse(hrData.Train$Prob>0.22,1,0)

table(hrData.Train$Attrition,hrData.Train$Class)
hrData.Test$Attrition=as.numeric(as.character(hrData.Test$Attrition))
hrData.Test_prediction1=hrData.Test[-c(31)]

compute.output=compute(nn1,hrData.Test_prediction1)
#hrData.Train$Prob=nn1$net.result[[1]]
hrData.Test$Predict.score = compute.output$net.result


library(ROCR)
pred <- ROCR::prediction(hrData.Test$Predict.score,hrData.Test$Attrition)
perf <- performance(pred, "tpr", "fpr")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = 2 * auc - 1
auc
KS
gini

hrData.Test$Class = ifelse(hrData.Test$Predict.score>0.22,1,0)
conf.matrix=table(hrData.Test$Attrition,hrData.Test$Class)

(conf.matrix[1]+conf.matrix[4])/882  #confusion matrix for Test

#rm(list=ls())
