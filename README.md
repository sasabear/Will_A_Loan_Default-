# Will A Loan Default

##Background 

Lending Club, located in San Francisco, is the worlds largest peer-to-peer lending platform. Online peer-to-peer lending is a relatively new practice of lending money without going through a traditional financial intermediary, such as a bank. However, compared to the traditional types of loans, peer-to-peer loans are usually unsecured personal loans and associated with high risk of default. Hence, our team is tempted to propose models to predict loan status (default/fully paid-off) based on the information before the loan initiation. Our approach could be served as a supplement tool to improve the company's overall risk control and to secure its long-term growth. 

##Objective

Our project is aimed to select relevant factors that have influential e↵ects on a loaners ability to repay his/her loan based on historical loan data. Relied on those factors, we apply statistical classification models to predict whether a loan will default. We will the performance of the models based on two indicators: total classification error rate and false negative rate. We intend to reduce false negative rate because we want to avoid the situation where many default observations are classified as non-default, which is very risky to the business. 

##Data Source 
 
Our analysis is based on the public data from Lending Club. The dataset contains complete loan data for all loans issued through the time period between 2013 and 2014, with 53 variables and 260,000 observations in total. After performing preliminary data cleaning (missing values and outliers) and features selection based on the objectives of project, a new dataset with 14 variables and 50,000 observations is constructed for further analysis.
