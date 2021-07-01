#Open the UCF Dataset 2018 - Training set.csv in Excel.
#Save the file as Subs2b.
#LTV, CoMonthlyRent and CoMonthlyLiability have a lot of NULLs and recorded as factor.
#Use the Excel Edit-Find-Replace All function to replace all NULL values in CoMonthlyLiability, LTV and CoMonthlyRent with 0.
#Save the file as Subs2b.

#Data Clearning on Subs2b
Subs2b <- read.csv("Google Drive/CFE Competition/Subs2b.csv", header = TRUE)

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

#Create a new dataset Subs3 with potential predictors.
key.var = c("LoanStatus", "LoanApproved", "ModifiedCreditScore", "ModifiedBankruptcyScore", "Source", "TotalEmpMon", "PrimeEmpMon", "EmploymentStatus", "TotalMonthlyIncome", "PrimeMonthlyIncome", "PrimeMonthlyRent", "AmountRequested", "TotalRent", "TotalMonthlyDebtBeforeLoan", "NumberOfOpenRevolvingAccounts", "VehicleMileage", "Loanterm", "OccupancyStatus", "RequestType", "DTI", "MemberIndicator", "TotalVehicleValue", "LTV", "isNewVehicle", "DownPayment", "OccupancyDuration", "EstimatedMonthlyPayment")
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

#####################################################################

#Decision Tree
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

#Decision tree pruning
set.seed(213)
trainset3$LoanApproved = as.factor(trainset3$LoanApproved)
cv.cfe = cv.tree(tree.cfe8, FUN=prune.misclass) #Running time: 2 minutes
cv.cfe

#Create a tree model with size 4
prune.cfe = prune.misclass(tree.cfe8, best=4)
plot(prune.cfe)
text(prune.cfe, pretty=0)

#Prediction with pruned tree
tree.pred = predict(prune.cfe, testset3, type="class")
confusionMatrix(data = tree.pred, reference = testset3$LoanApproved)

#############################################################################

#GBM Model
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
boost.3e = gbm(LoanApproved~ModifiedCreditScore+ModifiedBankruptcyScore+Source+PrimeEmpMon+TotalMonthlyIncome+PrimeMonthlyIncome+PrimeMonthlyRent+AmountRequested+TotalRent+TotalMonthlyDebtBeforeLoan+NumberOfOpenRevolvingAccounts+VehicleMileage+Loanterm+OccupancyStatus+DTI+MemberIndicator+RequestType+TotalVehicleValue+LTV+DownPayment+OccupancyDuration+EstimatedMonthlyPayment, data=trainset3, distribution="bernoulli", n.trees=2280, interaction.depth=5)

#Prediction of the tuned model
boost.3e.pred = predict(boost.3e, newdata=testset3, type="response", n.trees=2280)
boost.3e.test = ifelse(boost.3e.pred < 0.5, "0", "1")
boost.3e.test = as.factor(boost.3e.test)
confusionMatrix(data=boost.3e.test, reference=testset3$LoanApproved)

summary(boost.3e)




##############################################################################

#Data Clearning on Testing Set
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
write.csv(Submission,'Google Drive/CFE Competition/Submission.csv')

##############################################################################################
