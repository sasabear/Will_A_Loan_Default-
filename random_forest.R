names(data)[10:12]<-c("Rent","Mortage","Home")
factor(data[,9])
factor(data[,10])
factor(data[,11])
factor(data[,12])
y<-data[,7]
x<-data[,-7]
#-----------------------------------
#Random Forest
#-----------------------------------
library(randomForest)
y<-factor(y)
#RF without Down-sampling
model<-randomForest(y~.,data=x)
plot(model,log="y")
sum(model$predicted==y)/length(y)
sum(model$predicted==y&y==0)/sum(y==0)
varImpPlot(model)
#RF with DS
model.ds<-randomForest(y~.,data=x,sampsize=c(9000,9000),strada=y)
varImpPlot(model.ds)
plot(model.ds,log="y")
sum(model.ds$predicted==y)/length(y)
sum(model.ds$predicted==y&y==0)/sum(y==0)
varImpPlot(model.ds)
#-----------------------------------
# AdaBoosting 
#-----------------------------------
library(adabag)
data$loan_status<-factor(data$loan_status)
model.boost<-boosting(loan_status~.,data<-data,mfinal=10,control = rpart.control(cp = -1))
importanceplot(model.boost)
#-----------------------------------
#Tree (not meaningful)
#-----------------------------------
library(tree)
model.tree<-rpart(y~.,data=x,control = rpart.control(cp = -1))
plot(model.tree,margin=0.1)
text(model.tree)
