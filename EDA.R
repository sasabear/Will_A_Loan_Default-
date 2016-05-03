load("~/Desktop/data_use.RData")
#################
# modify the data
#################
# type(int_rate) from string to numeric
data_use$int_rate = as.numeric(gsub("%", "", data_use$int_rate))/100
# type(loan_status) from factor to string
data_use$loan_status = as.character(data_use$loan_status)
# type(loan_status) from string to numeric (0,1) 1:default+chargedOff; 0:fulliyPaid
data_use$loan_status = as.numeric(gsub("Fully Paid", "0",gsub("Default", "1",gsub("Charged Off", "1", data_use$loan_status))))
attach(data_use)
#################
# EDA
#################
par(mfrow=c(1,1))
# plot two category x's -----------------------------------------------------------------------------
plot(grade,as.factor(loan_status))
plot(home_ownership,as.factor(loan_status))
plot(emp_length,as.factor(loan_status))
plot(term,as.factor(loan_status))
plot(purpose,as.factor(loan_status))
# if drop the debt_consolidation (to see the others)
plot(subset(data_use,purpose!='debt_consolidation')$purpose,as.factor(subset(data_use,purpose!='debt_consolidation')$loan_status))
# plot two numerical x's and some linear regression (but not good)-----------------------------------
par(mfrow=c(2,2))
plot(annual_inc,dti)
plot(annual_inc,int_rate)
plot(annual_inc,loan_amnt)
plot(annual_inc,log(pub_rec))
summary(lm(annual_inc~pub_rec)) 
summary(lm(annual_inc~int_rate))
summary(lm(annual_inc~loan_amnt))
# log(u/1-u) ~ grade(A-F) ---------------------------------------------------------------------------
## (1)grade group
grades = as.character(sort(unique(data_use$grade)))
dti = as.character(sort(unique(data_use$grade)))
y = vector("numeric",length(grades))
for (i in 1:length(grades)){
  datai = subset(data_use,data_use$grade==grades[i])
  xi = nrow(subset(datai,datai$loan_status==1))/nrow(datai)
  y[i] = log(xi/(1-xi))
}
plot(y)
## (2)dti group (see Yuhui's code)
# Four graphs (see Yuhui's code)---------------------------------------------------------------------
dataFour <- data.frame(grade = as.numeric(data_use$sub_grade),
                       interestRate = data_use$int_rate,
                       term = as.character(term),
                       loanStatus = loan_status)
data11 = subset(dataFour,as.character(dataFour$term)==" 60 months" & dataFour$loanStatus==1)  
data12 = subset(dataFour,as.character(dataFour$term)==" 60 months" & dataFour$loanStatus==0) 
data21 = subset(dataFour,as.character(dataFour$term)==" 36 months" & dataFour$loanStatus==1)  
data22 = subset(dataFour,as.character(dataFour$term)==" 36 months" & dataFour$loanStatus==0)  
par(mfrow=c(2,2))
plot(data11$grade,data11$interestRate)
plot(data12$grade,data12$interestRate)
plot(data21$grade,data21$interestRate)
plot(data22$grade,data22$interestRate)

###############
# data groups----------------------------------------------------------------------------------------
# numeric data
dataNum <- data_use[,c(1,2,7,8,10)]
# ordinary data
dataOrd <- data_use[,c(3,4,12,13)]
# category data
dataCat <- data_use[,c(5,11)]
# Y
dataY <- data_use[,9]

# for the numeric data ------------------------------------------------------------------------------
names(dataNum)
# boxplot
par(mfrow=c(1,1))
## the annual_inc has some outliers () here drop 3 data points
boxplot(dataNum$annual_inc)
# use which.max() to get the index of the outliers
boxplot(dataNum$annual_inc[-11605])
boxplot(dataNum$annual_inc[-11605][-53525][-6780])
## could delete the outliers to get the distribution
boxplot(dataNum$pub_rec)
boxplot(dataNum$dti)
boxplot(subset(dataNum,dataNum$int_rate<0.25)$int_rate) # drop outliers
boxplot(dataNum$loan_amnt)
### check the outliers in defferent groups for int_rate!!!
boxplot(subset(data_use,data_use$grade=="A")$int_rate)
boxplot(subset(data_use,data_use$grade=="B")$int_rate)
boxplot(subset(data_use,data_use$grade=="C")$int_rate)
boxplot(subset(data_use,data_use$grade=="D")$int_rate) # one outlier
boxplot(subset(data_use,data_use$grade=="E")$int_rate) # one outlier
boxplot(subset(data_use,data_use$grade=="F")$int_rate)
boxplot(subset(data_use,data_use$grade=="G")$int_rate)
# histogram & Anderson-Darling test --all non-normal--so,use the nonparametric method
hist(dataNum$annual_inc)
hist(dataNum$pub_rec)
hist(dataNum$dti)
hist(dataNum$int_rate)
hist(dataNum$loan_amnt)
lines(density(dataNum$dti))
library(nortest)
ad.test(dataNum$int_rate)
ad.test(dataNum$annual_inc)
ad.test(dataNum$pub_rec)
ad.test(dataNum$dti)
ad.test(dataNum$int_rate)
ad.test(dataNum$loan_amnt)
# devide by term to do Wilcoxon Rank-Sum test
## dti
X36 = subset(data_use,as.character(term) == " 36 months")$dti
X60 = subset(data_use,as.character(term) == " 60 months")$dti
wilcox.test(X36,X60) # p-value < 2.2e-16 alternative hypothesis: true location shift is not equal to 0
ks.test(X36,X60) # p-value < 2.2e-16 alternative hypothesis: two-sided
## int_rate
X36_int_rate = subset(data_use,as.character(term) == " 36 months")$int_rate
X60_int_rate = subset(data_use,as.character(term) == " 60 months")$int_rate
wilcox.test(X36_int_rate,X60_int_rate) # p-value < 2.2e-16 alternative hypothesis: true location shift is not equal to 0
ks.test(X36_int_rate,X60_int_rate) # p-value < 2.2e-16 alternative hypothesis: two-sided
## loan_amnt
X36_loan_amnt = subset(data_use,as.character(term) == " 36 months")$loan_amnt
X60_loan_amnt = subset(data_use,as.character(term) == " 60 months")$loan_amnt
wilcox.test(X36_loan_amnt,X60_loan_amnt) # p-value < 2.2e-16 alternative hypothesis: true location shift is not equal to 0
ks.test(X36_loan_amnt,X60_loan_amnt) # p-value < 2.2e-16 alternative hypothesis: two-sided
### could add more EDAs (*^_^*)