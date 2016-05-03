load("~/Documents/Columbia/4201/project/code/adaData.RData")

#delete ownship "any"
data = data[-12485,]

#change grade
temp = rep(0,length(data[,1]))
temp[which(data[,4] == "A")] = 7
temp[which(data[,4] == "B")] = 6
temp[which(data[,4] == "C")] = 5
temp[which(data[,4] == "D")] = 4
temp[which(data[,4] == "E")] = 3
temp[which(data[,4] == "F")] = 2
temp[which(data[,4] == "G")] = 1
data[,4] = temp

#change paid or not
temp = rep(0,length(data[,1]))
temp[which(data[,9] == "Fully Paid")] = 1
temp[which(data[,9] != "Fully Paid")] = 0
data[,9] = temp

#delete length
data = data[-which(data[,3] == "n/a"),]

#change emp_length
temp = rep(0,length(data[,1]))
temp[which(data[,3] == " < 1 year")] = 0
temp[which(data[,3] == "1 year")] = 1
temp[which(data[,3] == "2 years")] = 2
temp[which(data[,3] == "3 years")] = 3
temp[which(data[,3] == "4 years")] = 4
temp[which(data[,3] == "5 years")] = 5
temp[which(data[,3] == "6 years")] = 6
temp[which(data[,3] == "7 years")] = 7
temp[which(data[,3] == "8 years")] = 8
temp[which(data[,3] == "9 years")] = 9
temp[which(data[,3] == "10+ years")] = 10
data[,3] = temp

#purpose column
temp = rep(0,length(data[,1]))
temp[which(data[,11] == "debt_consolidation")] = 1
temp[which(data[,11] != "debt_consolidation")] = 0
data[,11] = temp

#ownership
temp = matrix(0,length(data[,1]),3)
temp[which(data[,5] == "RENT"),1] = 1
temp[which(data[,5] == "MORTGAGE"),2] = 1
temp[which(data[,5] == "OWN"),3] = 1
data[,14] = temp[,1]
data[,15] = temp[,2]
data[,16] = temp[,3]

#interest rate
temp = rep(0,length(data[,1]))
temp = as.numeric(substr(as.character(data[,7]),2,6))/100
data[,7] = temp

data = data[,-5]
data = data[,-5]
data = data[,-10]
data = data[,-10]

#Logistic Regression
x = as.matrix(data[,c(1,2,3,4,5,6,8,9,10,11,12)])
y = data[,7]
set.seed(123)
fit = cv.glmnet(x,y,family="binomial",nfolds=5)
fit1 = glmnet(x, y,family="binomial",lambda = 0.0007711143)
fit2 <- glm(y~x, family = binomial("logit"))

sum(residuals(fit2, type = "pearson")^2)
deviance(fit2)
1 - pchisq(deviance(fit2), df.residual(fit2))


z = predict(fit2,newdata=data.frame(x),type="response")
z = as.numeric(z)
ans0 = rep(0,100)
ans1 = rep(0,100)
ansT = rep(0,100)
for (i in c(1:100)){
  temp = rep(0,length(z))
  temp[which(z<=i/100)] = 0
  temp[which(z>i/100)] = 1
  ans0[i] = sum(y[which(temp==0)]==0)/sum(y==0)
  ans1[i] = sum(y[which(temp==1)]==1)/sum(y==1)
  ansT[i] = (sum(y[which(temp==0)]==0)+sum(y[which(temp==1)]==1))/length(y)
}




