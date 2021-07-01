#Open the UCF Dataset 2018 - Training set.csv in Excel.
#Save the file as Subs2b.
#LTV, CoMonthlyRent and CoMonthlyLiability have a lot of NULLs and recorded as factor.
#Use the Excel Edit-Find-Replace All function to replace all NULL values in CoMonthlyLiability, LTV and CoMonthlyRent with 0.
#Save the file as Subs2b.

#####################################################################
#Beginning of Data Clearning on Subs2b
#####################################################################

setwd("C:/VanduWork/ucf/cfe")      #set the directory path
Subs2b <- read.csv("Subs2b.csv", header = TRUE, na.strings = c('NULL','NA'))

#Convert LoanStatus to a dummy variable: LoanApproved, add the new column
LoanApproved=rep('1', nrow(Subs2b))
LoanApproved[Subs2b$LoanStatus=='Declined']='0'
Subs2b = data.frame(Subs2b, LoanApproved)

#Delete Rows with Employed Months >1000, 4 rows removed.
sum(Subs2b$EmployedMonths>1000)
Subs2b = subset(Subs2b, !(Subs2b$EmployedMonths>1000))

#Combine Employ Months, Compare employment months between Applicants and CoApplicants, choose the higher..
PrimeEmpMon <- Subs2b$EmployedMonths + Subs2b$PrevEmployedMonths
CoEmpMon <- Subs2b$CoEmployedMonths + Subs2b$CoPrevEmployedMonths
TotalEmpMon = ifelse(PrimeEmpMon > CoEmpMon, PrimeEmpMon, CoEmpMon)
Subs2b = data.frame(Subs2b, PrimeEmpMon,TotalEmpMon)

#Remove Extreme Values
Subs2b = subset(Subs2b, !(PrimeMonthlyRent > 12000)) #1 row was removed.
Subs2b = subset(Subs2b, !(PrimeEmpMon > 766)) #1 row was removed.
Subs2b = subset(Subs2b, !(TotalEmpMon > 766)) #1 row was removed.

#Combine Rent
TotalRent = Subs2b$PrimeMonthlyRent + Subs2b$CoMonthlyRent
Subs2b = data.frame(Subs2b, TotalRent)

#Cleaning finish, save the file
write.csv(Subs2b,'Google Drive/CFE Competition/Subs2b.csv')

######################################################################
#Alternative, you can use our attached Subs2b to start from here.
######################################################################

#Create a new dataset Subs3 with potential predictors.
key.var = c("LoanStatus", "LoanApproved", "ModifiedCreditScore", "ModifiedBankruptcyScore", "Source", "TotalEmpMon", "PrimeEmpMon", "EmploymentStatus", "TotalMonthlyIncome", "PrimeMonthlyIncome", "PrimeMonthlyRent", "AmountRequested", "TotalRent", "TotalMonthlyDebtBeforeLoan", "NumberOfOpenRevolvingAccounts", "VehicleMileage", "Loanterm", "OccupancyStatus", "RequestType", "DTI", "MemberIndicator", "TotalVehicleValue", "LTV", "isNewVehicle", "DownPayment", "OccupancyDuration", "EstimatedMonthlyPayment", "CoApplicantIndicator","EmployedMonths")
Subs3 = Subs2b[key.var]

#Delete rows has na
row.has.na = apply(Subs3, 1, function(Subs3){any(is.na(Subs3))})
sum(row.has.na)
Subs3 = Subs3[!row.has.na, ] #214 rows deleted

#Calculate VIF
library(car)
glm.fit3 = glm(LoanApproved~ModifiedCreditScore+ModifiedBankruptcyScore+Source+TotalEmpMon+PrimeEmpMon+TotalEmpMon+EmploymentStatus+TotalMonthlyIncome+PrimeMonthlyIncome+PrimeMonthlyRent+AmountRequested+TotalRent+TotalMonthlyDebtBeforeLoan+NumberOfOpenRevolvingAccounts+VehicleMileage+Loanterm+OccupancyStatus+DTI+MemberIndicator+RequestType+TotalVehicleValue+LTV+isNewVehicle+DownPayment+OccupancyDuration+EstimatedMonthlyPayment, data=Subs3, family=binomial)
round(vif(glm.fit3), 2)

#Remove rows with high leverage values.
hv = as.data.frame(hatvalues(glm.fit3))
hv.avg = (26)/109633 #calculate average hv values in the dataset, (p+1)/n
hv.times = hatvalues(glm.fit3)/hv.avg
hv = data.frame(hv, hv.times)
Subs3 = data.frame(Subs3, hv.times)
sum(hv.times>100)
Subs3 = subset(Subs3, !(hv.times > 100)) #49 rows removed.
Subs3$hv.times = NULL

###Plots of categorical variables
library(tidyverse)

#Membership and ModifiedCreditScore
ggplot(data=Subs3) + geom_density(aes(x=ModifiedCreditScore, fill = MemberIndicator), alpha=0.4) + ggtitle("Membership and Modified Credit Score") + theme(plot.title = element_text(lineheight=.8, face="bold")) + xlim(400, 900) + geom_vline(data=Subs2b, aes(xintercept=mean(Subs2b$ModifiedCreditScore), color=MemberIndicator), linetype="dashed")

#Source and ModifiedCreditScore
ggplot(data=Subs3) + geom_density(aes(x=ModifiedCreditScore, fill = Source), alpha=0.4) + ggtitle("Source and Modified Credit Score") + theme(plot.title = element_text(lineheight=.8, face="bold")) + xlim(400, 900) + geom_vline(data=Subs2b, aes(xintercept=mean(Subs2b$ModifiedCreditScore), color=Source), linetype="dashed")

#OccupancyStatus and ModifiedCreditScore
ggplot(Subs3, aes(x=OccupancyStatus, y=ModifiedCreditScore)) + geom_boxplot(aes(fill=LoanStatus)) + theme(panel.grid.minor = element_line(size=0.5)) + scale_y_continuous(minor_breaks = seq(400, 900, 20)) + ylim(400, 900)+ geom_hline(yintercept=641) + ggtitle("OccupancyStatus and Modified Credit Score") + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(legend.position = "bottom")

#RequestType and ModifiedCreditScore
ggplot(Subs3, aes(x=RequestType, y=ModifiedCreditScore)) + geom_boxplot(aes(fill=LoanStatus)) + theme(panel.grid.minor = element_line(size=0.5)) + scale_y_continuous(minor_breaks = seq(400, 900, 20)) + ylim(400, 900)+ geom_hline(yintercept=641) + ggtitle("RequestType and Modified Credit Score") + theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(legend.position = "bottom")



#Split Subs3 to trainset3 and testset3
set.seed(751)
train.index <- sample(1:nrow(Subs3), nrow(Subs3)*0.75)
trainset3 = Subs3[train.index, ]
testset3 = Subs3[-train.index, ]

trainset10 = trainset3
testset10 = testset3
trainset10$LoanStatus <- NULL
testset10$LoanStatus <- NULL

library(gridExtra)
library(grid)
#####################################################################
#End of Data Cleaning 
#####################################################################



####################################################################
#Logistic Regression - run time less than a minute
####################################################################

glm.fit=glm(LoanApproved~.,data=trainset10,family = "binomial")
glm.probs=predict(glm.fit, testset10, type="response")
glm.pred=rep(0, nrow(testset10))
glm.pred[glm.probs >0.5] = 1


#confusion matrix
table(glm.pred, testset10$LoanApproved)

#test error
LRte = 1 - mean(glm.pred==testset10$LoanApproved)
cat("Logistic Regression testing error is ", LRte)


#hybrid approach logistic regression
#test error vector to track the test error for each threshold range
te=rep(0,10)

#percentModerateData vector tracks the percentage of data that was flagged "Moderate"
percentModerateData=rep(0,10)

#The first element in test error vector is the test error for logistic regression without using a threshold range
te[1] = 1 - mean(glm.pred==testset10$LoanApproved)
try=rep(0, length(glm.probs))
high = 0.55
low = 0.45
for ( i in 2:10) 
{
  try = glm.probs
  try[glm.probs >high] = 1
  try[glm.probs <low] = 0
  tryTestLoanApp = testset10$LoanApproved[try==1 | try==0]
  tryGlmPred = try[(try==1 | try==0)]
  table(tryGlmPred, tryTestLoanApp)
  te[i]=1 - mean(tryGlmPred==tryTestLoanApp)
  percentModerateData[i] = round((1-length(tryGlmPred)/length(glm.probs))*100)
  
  #widen the threshold range for the next run
  high = high + 0.05
  low = low - 0.05
  
}

te #test error
percentModerateData #percent of total data
moderateThreshold <- c("50","45-55","40-60","35-65","30-70","25-75","20-80","15-85","10-90","5-95")
LRHybridResult <- cbind(moderateThreshold, te, percentModerateData)
grid.table(LRHybridResult)

####################################################################
#End of Logistic Regression
####################################################################


#############################################################
#SVM - runtime under 3 minutes
##############################################################

library(kernlab)

#rbfdot (radial) kernel (test error = 15.8%)
svm2.pred <- ksvm(as.factor(LoanApproved) ~ ., 
                  data = trainset10, kernel = "rbfdot", type = "C-svc")
svm2.pred
predictedY2 <- predict(svm2.pred, testset10)

#confusion matrix
table(predictedY2, testset10$LoanApproved)
#test error
SVMte = 1 - mean(predictedY2==testset10$LoanApproved) #0.1566652 with no scaling
cat("SVM testing error is ", SVMte)
##############################################################
#End of SVM 
##############################################################

##############################################################
#GAM     runtime under 10minutes
##############################################################
library(gam)
kv=vector(,20)
te=vector(,20)
i=1
#smooth spline for each variable
fit.creditb=smooth.spline(trainset10$ModifiedCreditScore, trainset10$LoanApproved, cv=TRUE)
df_ModifiedCreditScore = fit.creditb$df
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore), family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='ModifiedCreditScore'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)

#DTI
i=i+1
fit.creditb=smooth.spline(trainset10$DTI, trainset10$LoanApproved, cv=TRUE)
df_DTI = fit.creditb$df
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='DTI'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#EstimatedMonthlyPayment
i=i+1
fit.creditb=smooth.spline(trainset10$EstimatedMonthlyPayment, trainset10$LoanApproved, cv=TRUE)
df_EstimatedMonthlyPayment = fit.creditb$df
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='EstimatedMonthlyPayment'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#AmountRequested
i=i+1
fit.creditb=smooth.spline(trainset10$AmountRequested, trainset10$LoanApproved, cv=TRUE)
df_AmountRequested = fit.creditb$df
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='AmountRequested'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#VehicleMileage
i=i+1
fit.creditb=smooth.spline(trainset10$VehicleMileage, trainset10$LoanApproved, cv=TRUE)
df_VehicleMileage = fit.creditb$df
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='VehicleMileage'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#MemberIndicator
i=i+1
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='MemberIndicator'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#LTV
i=i+1
fit.creditb=smooth.spline(trainset10$LTV, trainset10$LoanApproved, cv=TRUE)
df_LTV = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                +s(LTV, df=df_LTV)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='LTV'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te
### test error goes up after adding LTV so remove LTV from model

#RequestType
i=i+1
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='RequestType'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#TotalMonthlyIncome
i=i+1
fit.creditb=smooth.spline(trainset10$TotalMonthlyIncome, trainset10$LoanApproved, cv=TRUE)
df_TotalMonthlyIncome = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='TotalMonthlyIncome'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#Loanterm
i=i+1
fit.creditb=smooth.spline(trainset10$Loanterm, trainset10$LoanApproved, cv=TRUE)
df_Loanterm = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='Loanterm'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#OccupancyStatus
i=i+1
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='OccupancyStatus'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#PrimeMonthlyIncome
i=i+1
fit.creditb=smooth.spline(trainset10$PrimeMonthlyIncome, trainset10$LoanApproved, cv=TRUE)
df_PrimeMonthlyIncome = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='PrimeMonthlyIncome'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#ModifiedBankruptcyScore
i=i+1
fit.creditb=smooth.spline(trainset10$ModifiedBankruptcyScore, trainset10$LoanApproved, cv=TRUE)
df_ModifiedBankruptcyScore = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='ModifiedBankruptcyScore'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#NumberOfOpenRevolvingAccounts
i=i+1
fit.creditb=smooth.spline(trainset10$NumberOfOpenRevolvingAccounts, trainset10$LoanApproved, cv=TRUE)
df_NumberOfOpenRevolvingAccounts = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='NumberOfOpenRevolvingAccounts'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#TotalVehicleValue
i=i+1
fit.creditb=smooth.spline(trainset10$TotalVehicleValue, trainset10$LoanApproved, cv=TRUE)
df_TotalVehicleValue = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='TotalVehicleValue'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#TotalMonthlyDebtBeforeLoan
i=i+1
fit.creditb=smooth.spline(trainset10$TotalMonthlyDebtBeforeLoan, trainset10$LoanApproved, cv=TRUE)
df_TotalMonthlyDebtBeforeLoan = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(TotalMonthlyDebtBeforeLoan, df=df_TotalMonthlyDebtBeforeLoan)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='TotalMonthlyDebtBeforeLoan'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te
###test error goes much higher after adding TotalMonthlyDebtBeforeLoan so remove this variable

#PrimeEmpMon
i=i+1
fit.creditb=smooth.spline(trainset10$PrimeEmpMon, trainset10$LoanApproved, cv=TRUE)
df_PrimeEmpMon = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='PrimeEmpMon'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#PrimeMonthlyRent
i=i+1
fit.creditb=smooth.spline(trainset10$PrimeMonthlyRent, trainset10$LoanApproved, cv=TRUE)
df_PrimeMonthlyRent = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='PrimeMonthlyRent'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#TotalRent
i=i+1
fit.creditb=smooth.spline(trainset10$TotalRent, trainset10$LoanApproved, cv=TRUE)
df_TotalRent = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='TotalRent'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te

#Source
i=i+1
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +Source
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='Source'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te
### test error goes up after adding source so remove it.

#OccupancyDuration
i=i+1
fit.creditb=smooth.spline(trainset10$OccupancyDuration, trainset10$LoanApproved, cv=TRUE)
df_OccupancyDuration = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +s(OccupancyDuration, df=df_OccupancyDuration)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='OccupancyDuration'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#DownPayment
i=i+1
fit.creditb=smooth.spline(trainset10$DownPayment, trainset10$LoanApproved, cv=TRUE)
df_DownPayment = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +s(OccupancyDuration, df=df_OccupancyDuration)
                +s(DownPayment, df=df_DownPayment)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='DownPayment'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te
#test error goes up after adding downpayment so remove it


#CoApplicantIndicator
i=i+1
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +s(OccupancyDuration, df=df_OccupancyDuration)
                +CoApplicantIndicator
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='CoApplicantIndicator'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te



#isNewVehicle
i=i+1
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +s(OccupancyDuration, df=df_OccupancyDuration)
                +CoApplicantIndicator
                +isNewVehicle
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='isNewVehicle'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#EmploymentStatus
i=i+1
fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +s(OccupancyDuration, df=df_OccupancyDuration)
                +CoApplicantIndicator
                +isNewVehicle
                +EmploymentStatus
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='EmploymentStatus'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te
###test error goes up so take out EmploymentStatus

#EmployedMonths
i=i+1
fit.creditb=smooth.spline(trainset10$EmployedMonths, trainset10$LoanApproved, cv=TRUE)
df_EmployedMonths = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +s(OccupancyDuration, df=df_OccupancyDuration)
                +CoApplicantIndicator
                +isNewVehicle
                +s(EmployedMonths, df=df_EmployedMonths)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='EmployedMonths'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te


#EmployedMonths
i=i+1
fit.creditb=smooth.spline(trainset10$EmployedMonths, trainset10$LoanApproved, cv=TRUE)
df_EmployedMonths = fit.creditb$df

fit.creditd=gam(LoanApproved~s(ModifiedCreditScore, df=df_ModifiedCreditScore)
                +s(DTI, df=df_DTI)
                +s(EstimatedMonthlyPayment, df=df_EstimatedMonthlyPayment)
                +s(AmountRequested, df=df_AmountRequested)
                +s(VehicleMileage, df=df_VehicleMileage)
                + MemberIndicator
                + RequestType
                +s(TotalMonthlyIncome, df=df_TotalMonthlyIncome)
                +s(Loanterm, df=df_Loanterm)
                + OccupancyStatus
                +s(PrimeMonthlyIncome, df=df_PrimeMonthlyIncome)
                +s(ModifiedBankruptcyScore, df=df_ModifiedBankruptcyScore)
                +s(NumberOfOpenRevolvingAccounts, df=df_NumberOfOpenRevolvingAccounts)
                +s(TotalVehicleValue, df=df_TotalVehicleValue)
                +s(PrimeEmpMon, df=df_PrimeEmpMon)
                +s(PrimeMonthlyRent, df=df_PrimeMonthlyRent)
                +s(TotalRent, df=df_TotalRent)
                +s(OccupancyDuration, df=df_OccupancyDuration)
                +CoApplicantIndicator
                +isNewVehicle
                +s(EmployedMonths, df=df_EmployedMonths)
                , family=binomial,  data=trainset10)
creditd.probs=predict(fit.creditd, testset10, type="response")
creditd.pred=rep("0", nrow(testset10))
creditd.pred[creditd.probs >0.5] = "1"
table(creditd.pred, testset10$LoanApproved)
kv[i]='EmployedMonths'
te[i]=1 - mean(creditd.pred==testset10$LoanApproved)
te
GAMLowestTE = te[i]
cat("GAM testing error is ", GAMLowestTE)

#GAMhybrid 
GAMte=rep(0,10)
GAMpercentModerateData=rep(0,10)
GAMte[1] = GAMLowestTE
try=rep(0, length(creditd.probs))
high = 0.55
low = 0.45
for ( i in 2:10) 
{
  try = creditd.probs
  try[creditd.probs >high] = 1
  try[creditd.probs <low] = 0
  tryTestLoanApp = testset10$LoanApproved[try==1 | try==0]
  tryGamPred = try[(try==1 | try==0)]
  table(tryGamPred, tryTestLoanApp)
  GAMte[i]=1 - mean(tryGamPred==tryTestLoanApp)
  GAMpercentModerateData[i] = round((1-length(tryGamPred)/length(glm.probs))*100)
  high = high + 0.05
  low = low - 0.05
  
}

GAMte
GAMpercentModerateData
moderateThreshold <- c(0,10,20,30,40,50,60,70,80,90)
moderateThreshold <- c("50","45-55","40-60","35-65","30-70","25-75","20-80","15-85","10-90","5-95")
GAMHybridResult <- cbind(moderateThreshold, GAMte, GAMpercentModerateData)
grid.table(GAMHybridResult)


##############################################################
#End of GAM
##############################################################


####################################################################
#Decision Tree
#####################################################################

library(rpart)
library(e1071)
library(caret)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(tree)

#A decision tree model with 7 predictors
tree.cfe6=rpart(LoanApproved~ModifiedCreditScore+ModifiedBankruptcyScore+PrimeMonthlyRent+NumberOfOpenRevolvingAccounts+Loanterm+DTI+OccupancyStatus, method="class", data=trainset3)

#Variable Importance
tree.cfe6$variable.importance

#Prediction of the tree model
rparty.test = predict(tree.cfe6, newdata=testset3, type="class")
confusionMatrix(data = rparty.test, reference = testset3$LoanApproved)

#Decision tree plot
fancyRpartPlot(tree.cfe6)

#Use tree function to create the same 7 predictor model for pruning
trainset3$LoanApproved = as.factor(trainset3$LoanApproved)
tree.cfe8=tree(LoanApproved~ModifiedCreditScore+ModifiedBankruptcyScore+PrimeMonthlyRent+NumberOfOpenRevolvingAccounts+Loanterm+DTI+OccupancyStatus, data=trainset3)

#Decision tree pruning
set.seed(213)
cv.cfe = cv.tree(tree.cfe8, FUN=prune.misclass) #Running time: 2 minutes
cv.cfe

#Create a tree model with size 4
prune.cfe = prune.misclass(tree.cfe8, best=4)

#Prediction with pruned tree
tree.pred = predict(prune.cfe, testset3, type="class")
confusionMatrix(data = tree.pred, reference = testset3$LoanApproved)

#############################################################################
#End of Decision Tree
#############################################################################

#############################################################################
#GBM Model
#############################################################################

library(gbm)
library(e1071)
library(caret)

#gbm can only handle numeric response value, need to convert from factor to numeric.
trainset3$LoanApproved = as.numeric(trainset3$LoanApproved)
summary(trainset3$LoanApproved)
trainset3$LoanApproved = trainset3$LoanApproved - 1 #ensure the values are 0, 1
summary(trainset3$LoanApproved)

#GBM model, running time: 5 minutes
boost.3d = gbm(LoanApproved~ModifiedCreditScore+ModifiedBankruptcyScore+Source+PrimeEmpMon+TotalMonthlyIncome+PrimeMonthlyIncome+PrimeMonthlyRent+AmountRequested+TotalRent+TotalMonthlyDebtBeforeLoan+NumberOfOpenRevolvingAccounts+VehicleMileage+Loanterm+OccupancyStatus+DTI+MemberIndicator+RequestType+TotalVehicleValue+LTV+DownPayment+OccupancyDuration+EstimatedMonthlyPayment, data=trainset3, distribution="bernoulli", n.trees=1000, interaction.depth=4)

#Partial Dependence Plot
plot(boost.3d, i="ModifiedCreditScore")
plot(boost.3d, i="DTI", xlim=c(0, 60))

#Prediction
boost.3d.pred = predict(boost.3d, newdata=testset3, type="response", n.trees=1000)
boost.3d.test = ifelse(boost.3d.pred < 0.5, "0", "1")
boost.3d.test = as.factor(boost.3d.test)
testset3$LoanApproved = as.factor(testset3$LoanApproved)
confusionMatrix(data=boost.3d.test, reference=testset3$LoanApproved)

##############################################################################
#GBM Tuning
library(caret)
library(gbm)
library(h2o)

#Create a subset with key predictors in the GBM model only: Subs4
key.var = c("LoanApproved", "ModifiedCreditScore", "ModifiedBankruptcyScore", "Source", "PrimeEmpMon", "TotalMonthlyIncome", "PrimeMonthlyIncome", "PrimeMonthlyRent", "AmountRequested", "TotalRent", "TotalMonthlyDebtBeforeLoan", "NumberOfOpenRevolvingAccounts", "VehicleMileage", "Loanterm", "OccupancyStatus", "RequestType", "DTI", "MemberIndicator", "TotalVehicleValue", "LTV", "DownPayment", "OccupancyDuration", "EstimatedMonthlyPayment")
Subs4 = Subs3[key.var]

h2o.init(nthreads=-1) #Connect to Cluster
Subs4.hex <- as.h2o(Subs4, destination_frame="Subs4.hex") #convert Subs4 to hex frame

#Split data into train, validation and test sets
splits <- h2o.splitFrame(data = Subs4.hex, ratios = c(0.6,0.2), destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

#Convert the response variable as factor, as h2o can handle factor only.
response <- "LoanApproved"
Subs4.hex[[response]] <- as.factor(Subs4.hex[[response]])

#Create predictors
predictors = c("ModifiedCreditScore", "ModifiedBankruptcyScore", "Source", "PrimeEmpMon", "TotalMonthlyIncome", "PrimeMonthlyIncome", "PrimeMonthlyRent", "AmountRequested", "TotalRent", "TotalMonthlyDebtBeforeLoan", "NumberOfOpenRevolvingAccounts", "VehicleMileage", "Loanterm", "OccupancyStatus", "RequestType", "DTI", "MemberIndicator", "TotalVehicleValue", "LTV", "DownPayment", "OccupancyDuration", "EstimatedMonthlyPayment")

###Cross Validation Using GBM
#Use early stopping to automatically tune the number of trees using the validation AUC.
#Use a lower learning rate, takes more trees to converge).
#Use stochastic sampling of rows and columns to improve generalization.
#Running time is about 5 minutes.
gbm2 <- h2o.gbm(x = predictors, y = response, training_frame = train, validation_frame = valid, ntrees = 10000, learn_rate=0.01, stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC", sample_rate = 0.8, col_sample_rate = 0.8, seed = 1234, score_tree_interval = 10)

#List details of the tuned model, best result is n.tree = 2280, interaction depth = 5
summary(gbm2)

#Use the tuned model paramenters to build a GBM model, running time: 10 minutes
#gbm can only handle numeric response value, need to convert from factor to numeric.
trainset3$LoanApproved = as.numeric(trainset3$LoanApproved)
summary(trainset3$LoanApproved)
trainset3$LoanApproved = trainset3$LoanApproved - 1 #ensure the values are 0, 1
summary(trainset3$LoanApproved)

boost.3e = gbm(LoanApproved~ModifiedCreditScore+ModifiedBankruptcyScore+Source+PrimeEmpMon+TotalMonthlyIncome+PrimeMonthlyIncome+PrimeMonthlyRent+AmountRequested+TotalRent+TotalMonthlyDebtBeforeLoan+NumberOfOpenRevolvingAccounts+VehicleMileage+Loanterm+OccupancyStatus+DTI+MemberIndicator+RequestType+TotalVehicleValue+LTV+DownPayment+OccupancyDuration+EstimatedMonthlyPayment, data=trainset3, distribution="bernoulli", n.trees=2280, interaction.depth=5)

#Prediction of the tuned model
boost.3e.pred = predict(boost.3e, newdata=testset3, type="response", n.trees=2280)
boost.3e.test = ifelse(boost.3e.pred < 0.5, "0", "1")
boost.3e.test = as.factor(boost.3e.test)
confusionMatrix(data=boost.3e.test, reference=testset3$LoanApproved)

summary(boost.3e)

#GBM Hybrid
BoostingLowestTE = 1 - mean(boost.3e.test==testset3$LoanApproved)

#histogram for Predicted probability distribution over misclassified loan stats
hist(boost.3e.pred[boost.3e.test!=testset3$LoanApproved], 
     xlab = "Predicted probabilities",
     ylab = "Number of misclassified loans",
     col="light blue")

hist(boost.3e.pred[boost.3e.test==testset3$LoanApproved], 
     xlab = "Predicted probabilities",
     ylab = "Number of correctly predicted loans",
     col="light pink")

Boostingte=rep(0,10)
BoostingpercentModerateData=rep(0,10)
Boostingte[1] = BoostingLowestTE
try=rep(0, length(boost.3e.pred))
high = 0.55
low = 0.45
for ( i in 2:10) 
{
  try = boost.3e.pred
  try[boost.3e.pred >high] = 1
  try[boost.3e.pred <low] = 0
  tryTestLoanApp = testset3$LoanApproved[try==1 | try==0]
  tryBoostingPred = try[(try==1 | try==0)]
  table(tryBoostingPred, tryTestLoanApp)
  Boostingte[i]=1 - mean(tryBoostingPred==tryTestLoanApp)
  BoostingpercentModerateData[i] = round((1-length(tryBoostingPred)/length(boost.3e.pred) )*100)
  high = high + 0.05
  low = low - 0.05
  
}

Boostingte
BoostingpercentModerateData
moderateThreshold <- c("50","45-55","40-60","35-65","30-70","25-75","20-80","15-85","10-90","5-95")
GBMHybridResult <- cbind(moderateThreshold, Boostingte, BoostingpercentModerateData)
grid.table(GBMHybridResult)

#############################################################################
#End of GBM
###############################################################


##############################################################################
#Data Clearning and GBM prediction on Test data
##############################################################################
#Use the Excel Edit-Find-Replace All functin to replace all NULL values in CoMonthlyLiability, LTV and CoMonthlyRent with 0.  Save the file as cfe.test in csv format
cfe.test <- read.csv("Google Drive/CFE Competition/cfe.test.csv", header = TRUE)

#Combine Employ Months, Compare employment months between Applicants and CoApplicants, choose the higher..
PrimeEmpMon <- cfe.test$EmployedMonths + cfe.test$PrevEmployedMonths
CoEmpMon <- cfe.test$CoEmployedMonths + cfe.test$CoPrevEmployedMonths
TotalEmpMon = ifelse(PrimeEmpMon > CoEmpMon, PrimeEmpMon, CoEmpMon)
cfe.test=data.frame(cfe.test, PrimeEmpMon,TotalEmpMon)

#Combine Rent
TotalRent = cfe.test$PrimeMonthlyRent + cfe.test$CoMonthlyRent
cfe.test = data.frame(cfe.test, TotalRent)

#Cleaning finish, save the file
write.csv(cfe.test,'Google Drive/CFE Competition/cfe.test.csv')

###########################################################################################
#Prediction with tuned GBM model for competition submission
library(gbm)
library(e1071)
library(caret)

boost.3e.pred = predict(boost.3e, newdata=cfe.test, type="response", n.trees=2280)
Prediction = ifelse(boost.3e.pred < 0.5, "0", "1") #0 is decline, 1 is approve
Prediction = as.factor(Prediction)
cfe.test = data.frame(cfe.test, Prediction)

#Complete prediction, save result
write.csv(cfe.test,'Google Drive/CFE Competition/cfe.test.csv')

#Create a Submission set, choose LoanNumber and Prediction from cfe.test
key.var = c("LoanNumber", "Prediction")
Submission = cfe.test[key.var]
write.csv(Submission,'Google Drive/CFE Competition/Result_ID_33.csv')

##############################################################################################
#End of R Code
##############################################################################################

