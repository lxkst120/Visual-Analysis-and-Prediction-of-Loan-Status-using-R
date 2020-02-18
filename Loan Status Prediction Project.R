# Required packages and libraries used for the project in R
install.packages("dplyr")
library(dplyr) 
install.packages("plyr")
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages(c("corrplot"))
library(corrplot)
install.packages(c("ggm", "gmodels", "vcd", "Hmisc",                
                   "pastecs", "psych", "doBy")) 
library(ggm)
library(Hmisc)
library(pastecs)
library(psych)
library(doBy)
library(vcd)
library(gmodels)
install.packages("car")
library(car)
install.packages('MASS')
library(MASS)

# This command is used to get the location of current working directory
getwd() 
# This command is used to point to the folder containing the required file   
setwd("U:/R/R project/Final Project")

#Read the file Loan.csv
#This command imports the required data set and saves it to the Loan data frame.
Loan <- read.csv("Loan.csv",header=TRUE)
Loan

# Structure of Loan dataframe to see if the data is structured or not
str(Loan)

#Checking for the structure and other possible incompleteness
summary(Loan)

sum(is.na(Loan))
#[1] 36
#The data set now has 36 missing values.

#Replacing the NA valus with mean values of the Loan Amount Variable
Loan$LoanAmount <- ifelse(is.na(Loan$LoanAmount),
                             ave(Loan$LoanAmount,FUN = function(x)mean(x,na.rm=TRUE)),
                             Loan$LoanAmount)

#Replacing the NA valus with mean values of the Loan Amount Term
Loan$Loan_Amount_Term <-ifelse(is.na(Loan$Loan_Amount_Term),
                                  ave(Loan$Loan_Amount_Term,FUN = function(x)mean(x,na.rm=TRUE)),
                               Loan$Loan_Amount_Term)

#Credit History is described whether or not customer meets guidelines. 
#Loan Status 1 for approved loan, 0 for rejected.

Loan$Credit_History = factor(Loan$Credit_History, levels = c(0,1), labels = c("Unmet", "Met"))
Loan$Loan_Status = as.numeric(Loan$Loan_Status) - 1

LoanData<-Loan

#save the file in our current working directory
write.table(LoanData,file="LoanData.csv",row.names=F,sep=",")

summary(LoanData)
str(LoanData)

sum(is.na(LoanData))
#[1] 0
#The data set now has 0 missing values.

#One-way contingency tables for the categorical variables.
# We can create simple frequency counts using the table() function in base R
#GENDER
table1 <- with(LoanData, table(Gender))
table1# frequencies
prop.table(table1)# proportions
prop.table(table1)*100 # percentages
addmargins(table1)

#MARRIED
table2 <- with(LoanData, table(Married))
table2# frequencies
prop.table(table2)# proportions
prop.table(table2)*100 # percentages
addmargins(table2)

#EDUCATION
table3 <- with(LoanData, table(Education))
table3# frequencies
prop.table(table3)# proportions
prop.table(table3)*100 # percentages
addmargins(table3)

#SELF-EMPLOYED
table4 <- with(LoanData, table(Self_Employed))
table4# frequencies
prop.table(table4)# proportions
prop.table(table4)*100 # percentages
addmargins(table4)

#CREDIT_HISTORY
table5 <- with(LoanData, table(Credit_History))
table5# frequencies
prop.table(table5)# proportions
prop.table(table5)*100 # percentages
addmargins(table5)

#PROPERTY-AREA
table6 <- with(LoanData, table(Property_Area))
table6# frequencies
prop.table(table6)# proportions
prop.table(table6)*100 # percentages
addmargins(table6)

#Two-way contingency tables for the categorical variables.
# Alternatively, the xtabs() function allows you to create a contingency 
# table using formula style input

#LOAN-STATUS & CREDIT-HISTORY
table7 <- xtabs(~ Loan_Status+Credit_History, data=LoanData)
table7
addmargins(table7)

#LOAN-STATUS & PROPERTY-AREA
table8 <- xtabs(~ Loan_Status+Property_Area, data=LoanData)
table8
addmargins(table8)

#LOAN-STATUS & SELF-EMPLOYED
table9 <- xtabs(~ Loan_Status+Self_Employed, data=LoanData)
table9
addmargins(table9)

#LOAN-STATUS & EDUCATION
table10 <- xtabs(~ Loan_Status+Education, data=LoanData)
table10
addmargins(table10)

#LOAN-STATUS & MARRIED
table11 <- xtabs(~ Loan_Status+Married, data=LoanData)
table11
addmargins(table11)

#LOAN-STATUS & GENDER
table12 <- xtabs(~ Loan_Status+Gender, data=LoanData)
table12
addmargins(table12)



# Chi-square test of independence

 
#It is used to determine whether there is a significant association between the two variables.

#The chi-square goodness of fit test is appropriate when the following conditions are met:
#The sampling method is simple random sampling.
#The variable under study is categorical.


#Degrees of freedom: The degrees of freedom (DF) is equal to the number of levels (k) of the categorical variable minus 1.
# DF = k - 1

#Expected frequency counts: The expected frequency counts at each level of the categorical variable
#are equal to the sample size times the hypothesized proportion from the null hypothesis
# Ei = npi
#where Ei is the expected frequency count for the ith level of the categorical variable, 
#n is the total sample size, and pi is the hypothesized proportion of observations in level i.

#Test statistic: The test statistic is a chi-square random variable defined by the following equation.
# chi-square=[ (Oi - Ei)2 / Ei ]
#where Oi is the observed frequency count for the ith level of the categorical variable, 
#and Ei is the expected frequency count for the ith level of the categorical variable.

#P-value: The P-value is the probability of observing a sample statistic as extreme as the test statistic


#H0: Variables X and Loan Status are independent

#Ha: Variables X and Loan Status are not independent

#If the P-value is less than the significance level (0.05), we cannot accept the null hypothesis.
#so, if p>0.05 then that Loan status is independent of that variable and need not consider
#that variable for further analysis.

#FOR GENDER VARIABLE
chisq.test(LoanData$Gender,LoanData$Loan_Status)

#	Pearson's Chi-squared test with Yates' continuity correction

#data:  LoanData$Gender and LoanData$Loan_Status
#X-squared = 0.27192, df = 1, p-value = 0.602
#We can say that Loan approval doesn't depend on gender

#FOR MARRIED VARIABLE
chisq.test(LoanData$Married,LoanData$Loan_Status)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  LoanData$Married and LoanData$Loan_Status
#X-squared = 3.989, df = 1, p-value = 0.0458
#It's apparent that Loan approval depends on Marital status


#FOR NO.OF DEPENDENTS VARIABLE
chisq.test(LoanData$Dependents,LoanData$Loan_Status)

#Pearson's Chi-squared test

#data:  LoanData$Dependents and LoanData$Loan_Status
#X-squared = 3.1514, df = 3, p-value = 0.3689

#We can say that Loan approval doesn't depend on Number of Dependents

#FOR EDUCATION VARIABLE
chisq.test(LoanData$Education,LoanData$Loan_Status)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  LoanData$Education and LoanData$Loan_Status
#X-squared = 4.0915, df = 1, p-value = 0.0431

#It's apparent that Loan approval depends on Education

#FOR SELF-EMPLOYED VARIABLE
chisq.test(LoanData$Self_Employed,LoanData$Loan_Status)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  LoanData$Self_Employed and LoanData$Loan_Status
#X-squared = 1.0223e-29, df = 1, p-value = 1
#Loan approval doesn't depend on if applicant is self employed


#FOR CREDIT-HISTORY VARIABLE
chisq.test(LoanData$Credit_History,LoanData$Loan_Status)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  LoanData$Credit_History and LoanData$Loan_Status
#X-squared = 112.7, df = 1, p-value < 2.2e-16

#It's apparent that Loan approval depends on Credit History

#FOR PROPERTY_AREA VARIABLE
chisq.test(LoanData$Property_Area,LoanData$Loan_Status)

#Pearson's Chi-squared test

#data:  LoanData$Property_Area and LoanData$Loan_Status
#X-squared = 12.298, df = 2, p-value = 0.002136

#It's apparent that Loan approval depends on Property area.


#Referring to p.values located in the table above, we can conclude that following significance level of 5% variables:
# Gender
#Dependents
#Self_Employed
#are independent of loan_status and therefore should give small predictive power in future model.


#FOR LOAN AMOUNT VARIABLE
chisq.test(LoanData$LoanAmount,LoanData$Loan_Status)
#Pearson's Chi-squared test

#data:  LoanData$LoanAmount and LoanData$Loan_Status
#X-squared = 205.53, df = 203, p-value = 0.4373

#Warning message:
#In chisq.test(LoanData$LoanAmount, LoanData$Loan_Status) :
#Chi-squared approximation may be incorrect

#Since the chi-square can't perform well on Loan Amount Variable
#I started performing various Statistical analysis on continuous variables

##########
#STATISTICAL ANALYSIS ON CONTINUOUS VARIABLES


mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  mi<-min(x)
  ma<-max(x)
  me<-median(x)
  IQR<-IQR(x,na.rm=FALSE,type=7)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(length=n, min=mi, max=ma, median=me, mean=m, IQR=IQR, stdev=s, skew=skew, kurtosis=kurt))
}

#Applicant Income
myvars <- c("ApplicantIncome")

aggregate(LoanData[myvars], by=list(Loan_Status=LoanData$Loan_Status), mystats)

#So we can easily note that Applicant Income has skewed distribution (median differs from mean)

#Density plot for Applicant Income
ggplot(LoanData, aes(x=ApplicantIncome,group=Loan_Status, fill = Loan_Status)) + geom_density(adjust=1.5, alpha = 0.2)
#From the above density plot, we can say that there are more applicants whose income is less than 20,000 rupees per month.

#Violin plot for Applicant Income
# violin plots give you an idea of the density of the data as well as outliers, that 
#Violin plots are similar to box plots,
#except that they also show the probability density of the data at different values
p <- ggplot(LoanData, aes(factor(Loan_Status), ApplicantIncome))
p + geom_violin()
#Violin plot after color grading
p + geom_violin(aes(fill = factor(Loan_Status)))

#Within prepared violin plot we can note that distribution for both subgroups looks very similar.
#Both have some outliers.

##############

#CoApplicant Income
myvars1 <- c("CoapplicantIncome")

aggregate(LoanData[myvars1], by=list(Loan_Status=LoanData$Loan_Status), mystats)
#So we can easily note that Coapplicant Income has skewed distribution (median differs from mean). 
#Very interesting is big difference between mean and median.

#Density plot for Coapplicant Income
ggplot(LoanData, aes(x=CoapplicantIncome,group=Loan_Status, fill = Loan_Status)) + geom_density(adjust=1.5, alpha = 0.2)
#From the above density plot, we can say that there are more applicants whose income is less than 10,000 rupees per month.

#Violin plot for Coapplicant Income
p <- ggplot(LoanData, aes(factor(Loan_Status), CoapplicantIncome))
p + geom_violin()
#Violin plot after color grading
p + geom_violin(aes(fill = factor(Loan_Status)))

#Visible is high number of coapplicants with income equal to 0.

############

#LoanAmount
myvars2 <- c("LoanAmount")

aggregate(LoanData[myvars2], by=list(Loan_Status=LoanData$Loan_Status), mystats)
#Similarly to Coapplicant Income, accepted loans subgroups is more numerous than rejected. 
#Median and means in both subgroups are very similar. 


#Density plot for LoanAmount
ggplot(LoanData, aes(x=LoanAmount,group=Loan_Status, fill = Loan_Status)) + geom_density(adjust=1.5, alpha = 0.2)
#Maximum amount with accepted loans is greater than maximum amount within rejected loans. 
#We could have expected that loans with greater amount are more likely to be rejected.

#Violin plot for LoanAmount
p <- ggplot(LoanData, aes(factor(Loan_Status), LoanAmount))
p + geom_violin()
#Violin plot after color grading
p + geom_violin(aes(fill = factor(Loan_Status))) 
#Accepted subgroup is more dense around 100 - 150. Rejected has higher IQR and both subgroups have some outliers.

############

#LoanAmountTerm
myvars3 <- c("Loan_Amount_Term")

aggregate(LoanData[myvars3], by=list(Loan_Status=LoanData$Loan_Status), mystats)
#Two subgroups have very similar distributions with difference within kurtosis.


#Density plot for LoanAmount Term
ggplot(LoanData, aes(x=Loan_Amount_Term,group=Loan_Status, fill = Loan_Status)) + geom_density(adjust=1.5, alpha = 0.2)
#From the above graph we can say that there are more applicants who loan amount term lies between 350-400 months. 
#Hence this region is densely populated.

#Violin plot for LoanAmount Term
p <- ggplot(LoanData, aes(factor(Loan_Status), Loan_Amount_Term))
p + geom_violin()
#Violin plot after color grading
p + geom_violin(aes(fill = factor(Loan_Status)))
#Minimum term of applied loans was 12 months. Maximum 480 months. 
#Applicants usually apply for loans with term close to 30 years.



#CORRELATION FOR CONTINUOUS VARIABLES

#Now is the time to analyze the continuous variables correlation. 
#It is crucial to track highly correlated variables in order to prevent multicollinearity prematurely.
# The Pearson correlation assesses the degree of linear relationship between two quantitative variables. 
#I will not use Pearson to calculate correlation coefficients as this method is highly sensitive 
#to non-normal distribution and outliers presence.
#I use Kendall's tau-b coefficient instead of Pearson's, 
#which is more effective in determining whether two non-parametric data samples are correlated with ties.



K = LoanData %>% select(ApplicantIncome, CoapplicantIncome, LoanAmount, Loan_Amount_Term) %>% na.omit()
K_cor = cor(K, method = "kendall")
K_cor

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(K_cor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)
#There are very few cont. variables so matrix is simple. We can see that:
#Applicant Income and Loan Amount are moderately correlated.
#Loan Amount and Coapplicant Income are negatively weakly associated.
#Coapplicant Income and Loan Amount are weakly associated.
#The rest is very weakly correlated.


#**************#

##CONVERTING THE DATA TO NUMERICALS TO PERFORM MULTIPLE REGRESSION ANALYSIS

LoanData <- read.csv("LoanData.csv",header=TRUE)
LoanData

# Structure of Loan dataframe to see if the data is structured or not
str(LoanData)

#recoding Gender for data where Male to 1 and Female to 0 
Loan_reg <- LoanData%>%  mutate(Gender= ifelse(Gender == "Male",1,0))
str(Loan_reg)

#recoding Marital status for data where Married="Yes" to 1 and Married="No" to 0
Loan_reg1 <- Loan_reg %>%  mutate(Married= ifelse(Married == "Yes",1,0))
str(Loan_reg1)

#recoding Education for data where Education="Graduate" to 1 and "Not Graduate" to 0
Loan_reg2 <- Loan_reg1 %>%  mutate(Education= ifelse(Education == "Graduate",1,0))
str(Loan_reg2)

#recoding Self_Employed for data where Self_Employed="Yes" to 1 and "No" to 0
Loan_reg3 <- Loan_reg2 %>%  mutate(Self_Employed= ifelse(Self_Employed == "Yes",1,0))
str(Loan_reg3)

#recoding Property_Area for data where Rural=0, Urban=1 and Semiurban=2
Loan_reg3$Property_Area
Loan_reg3$Property_Area = factor(Loan_reg3$Property_Area,levels =c('Rural', 'Urban', 'Semiurban'),labels = c(0, 1, 2))
str(Loan_reg3)

#recoding Credit_History for data where Credit_History="Met" to 1 and "Unmet" to 0
Loan_reg4 <- Loan_reg3 %>%  mutate(Credit_History= ifelse(Credit_History == "Met",1,0))
Loan_model <-Loan_reg4
str(Loan_model)

#save the file in our current working directory
write.table(Loan_model,file="Loan_model.csv",row.names=F,sep=",")

#When you need to look at several plots, such as at the beginning of a multiple regression analysis, 
#a scatter plot matrix is a very useful tool.
scatterplotMatrix(formula=~CoapplicantIncome+LoanAmount+Loan_Amount_Term+ApplicantIncome, data=Loan_model, diagonal="histogram")

#As seen in the Violin and scatter plots the ApplicantIncome,CoapplicantIncome and LoanAmount has outliers
# and we are treating these factors to improve the performance

#############
#OUTLIER TREATMENT

#Outlier Treatment for ApplicantIncome
bench <- 5795 + 1.5*IQR(Loan_model$ApplicantIncome) #Q3 + 1.5*IQR(data$Age)
bench

#WINsORIZING method of treating outlier
Loan_model$ApplicantIncome[Loan_model$ApplicantIncome > bench]

Loan_model$ApplicantIncome[Loan_model$ApplicantIncome > bench] <- bench
summary(Loan_model$ApplicantIncome)

boxplot(Loan_model$ApplicantIncome, main = "Boxplot for ApplicantIncome",ylab="Applicant Income ",col=(c("gold")))


#Outlier Treatment for CoapplicantIncome
bench1 <- 2297 + 1.5*IQR(Loan_model$CoapplicantIncome) #Q3 + 1.5*IQR(data$Age)
bench1

#WINsORIZING method of treating outlier
Loan_model$CoapplicantIncome[Loan_model$CoapplicantIncome > bench1]

Loan_model$CoapplicantIncome[Loan_model$CoapplicantIncome > bench1] <- bench1
summary(Loan_model$CoapplicantIncome)

boxplot(Loan_model$CoapplicantIncome, main = "Boxplot for CoapplicantIncome",ylab="Coapplicant Income ",col=(c("lightblue")))


#Outlier Treatment for LoanAmount 
bench2 <- 164.8 + 1.5*IQR(Loan_model$LoanAmount) #Q3 + 1.5*IQR(data$Age)
bench2

#WINsORIZING method of treating outlier
Loan_model$LoanAmount [Loan_model$LoanAmount  > bench2]

Loan_model$LoanAmount [Loan_model$LoanAmount  > bench2] <- bench2
summary(Loan_model$LoanAmount )

boxplot(Loan_model$LoanAmount, main = "Boxplot for LoanAmount",ylab="Loan Amount in Thousands of Rupees ",col=(c("lightgreen")))

#The outliers have all been treated and the data is now clean to an appreciable level.

##**********MULTIPLE REGRESSION ANALYSIS

# performing Multiple linear regression between Loan_Status and all variables
#to evaluate the model performance.

Loan_pef <- lm(Loan_Status ~ Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+
            LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,
          data = Loan_model)
summary(Loan_pef)


#The summary statistics above tells us a number of things.
#We can consider a linear model to be statistically significant only when these p-Values are less. 
#Higher the t-value, the better the model is. 
#The t-statistic is the coefficient estimate divided by the standard error. If your regression is based on what 
#statisticians call a "large" sample (30 or more observations), a t-statistic greater than 2 (or less than -2) 
#indicates the coefficient is significant with >95% confidence.
#A predictor that has a low p-value is likely to be a meaningful addition to your model 
#because changes in the predictor's value are related to changes in the response variable.
#Conversely, a larger (insignificant) p-value suggests that changes in the predictor are not associated with changes in the response.
#From our summary we see that p-value of 'Self_Employed' and 'Dependents' is high and t-value is low so we will try eliminating that variables
#and see if our model accuracy is improved or not.
#Residual Standard error is 0.4141, that is deviation from getting perfect linear regression.
#R-squared is a statistical measure of how close the data are to the fitted regression line. 
#It is also known as the coefficient of determination, or the coefficient of multiple determination for multiple regression.
#The definition of R-squared is fairly straight-forward; it is the percentage of the response variable variation that is explained by a linear model.
#R^2 and Adj R^2 gives accuracy of model, we will consider Adj R^2 to be more accurate as R^2 changes with added variables. 
#In general, the higher the R-squared, the better the model fits your data.
#Our model accuracy at this point is 20.34%. 
#The F-test of the overall significance is a specific form of the F-test.
#F-value gives over all performance of the model that is 14.05.


############## Simple Regression Diagnostics ####################

# one way to evaluate the statistical assumptions in regression analysis
# is to plot the results of lm

par(mfrow = c(2, 2))
plot(Loan_pef)



#Removing Self_Employed and Dependents variables

Loan_pef1 <- lm(Loan_Status ~ Gender+Married+Education+ApplicantIncome+
                  CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,
               data = Loan_model)
summary(Loan_pef1)


#By removing Self_Employed and Dependents our model accuracy(Adj R^2) has increased to 20.61% from 20.34%. 
#Residual Standard error has also reduced from 0.4141 to 0.4134. 
#F-value (higher the better) increased to 16.91 from 14.05. 
#We can see from above results that, p-value for Gender is very high 
#so we will remove that variables from our model in next step and see if it improves our model.


par(mfrow = c(2, 2))
plot(Loan_pef1)


#Removing Gender and Loan_Amount_Term variables

Loan_pef2 <- lm(Loan_Status ~ Married+Education+ApplicantIncome+CoapplicantIncome+
                  LoanAmount+Credit_History+Property_Area,
                data = Loan_model)
summary(Loan_pef2)


#By removing gender and Loan_Amount_Term variables our model accuracy(Adj R^2) has increased to 20.85% from 20.74%. 
#Residual Standard error has also reduced from 0.4131 to 0.4128. 
#F-value (higher the better) increased to 21.18 from 18.82. 
#We can see from above results that, p-value for ApplicantIncome and Coapplicant Income is moderately high 
#so we will remove that variables from our model in next step and see if it improves our model.


par(mfrow = c(2, 2))
plot(Loan_pef2)



#Removing ApplicantIncome and Coapplicant Income variables
Loan_pef3 <- lm(Loan_Status ~ Married+Education+
                  LoanAmount+Credit_History+Property_Area,
                data = Loan_model)
summary(Loan_pef3)

#By removing ApplicantIncome and Coapplicant Income variables our model accuracy(Adj R^2) has increased to 20.96% from 20.85%. 
#Residual Standard error has also reduced from 0.4128 to 0.4125. 
#F-value (higher the better) increased to 28.1 from 21.18.

#We have improved accuracy of our model from 20.34% to 20.96%., with reducing the error rate 
#and increasing the overall performance (F-statistic) of the model. 
#It is a good practice to bring error rate to 0 and our model has its low error value. 
#And accuracy depends on the data we take and always cannot get high accuracy when we study behavioral data. 
#We see that Loan Status has a strong relation to Credit History, Married, Property area, Loan Amount and Education.

par(mfrow = c(2, 2))
plot(Loan_pef3)

##  Evaluating multi-collinearity

# Multicollinearity can be detected using the variance inflation factor(VIF). 
#For any predictor variable, the square root of the VIF indicates the 
# degree to which the confidence interval for that variables regression parameter is expanded
# relative to a model with uncorrelated predictors.
# VIF values are provided by the vif() function in the car package.
# As a general rule, a sqrt >2
# indicates a multicollinearity problem.
vif(Loan_pef3)
sqrt(vif(Loan_pef3)) > 2

#No multi collinearity here


############  Backward stepwise selection  ##############

# The stepAIC() function in the MASS package performs
# stepwise model selection (forward, backward, stepwise) using the AIC critera
# The Akaike Information Criterion (AIC) is a method for comparing
# models. The index takes into account a model's statistical fit and the number of
# parameters needed to achieve this fit. 

# Models with smaller AIC values indicating adequate fit with fewer parameters are preferred.

library(MASS)
Loan_backward<- lm(Loan_Status ~ Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+
                      LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,
                    data = Loan_model)
 
# backward direction
stepAIC(Loan_backward, direction = "backward")

# We start with all 11 predictors in the model. 
# For each backward step, the AIC column provides the model AIC resulting from the deletion 
# of the variable listed in that row. 
# As we can see when each variable is being removed the AIC value keeps on decreasing from 
# from -1069.82 to -1080.49. 
# Deleting any more variables would increase the AIC, so the process stops.
#Negative AIC indicates less information loss than a positive AIC and therefore a better model.


#Checking for class imbalance
prop.table(table(Loan_model$Loan_Status))

#      0         1 
#0.3127036 0.6872964 

table(Loan_model$Loan_Status)

# 0   1 
#192 422

#Class imbalance is a situation, mostly in classification model building; where the total number of
#positive class of a data set is extremely lower than the total number of the negative class.

#In the data set, we have 68.7% of the response variable as YES and 31.3% as NO.
#Hence, we can conclude that there is no class imbalance in this data set.

# Splitting into Train and Test Data
set.seed(222)
split = sample(2,nrow(Loan_model),prob = c(0.75,0.25),replace = TRUE)
train_set = Loan_model[split == 1,]
test_set = Loan_model[split == 2,]

#It is the usual practice in Machine Learning field to divide the data set into train and test set. 
#The model will be built on the train set and the performance of the model will be tested on the test.


#checking dimensions of train and test data sets
dim(train_set)
dim(test_set)


##**************************
#LOGISTIC REGRESSION

#Logistic regression uses sigmoid function to classify variables into classes 
#and its basically applicable to classification problems

# Fitting Logistic Regression to the Training set
logistics_classifier = glm(formula = Loan_Status ~ .,
                           family = binomial,
                           data = train_set[,-c(1)])

summary(logistics_classifier)


#Call:
#  glm(formula = Loan_Status ~ ., family = binomial, data = train_set[, -c(1)])

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.3670  -0.8250   0.5534   0.7176   1.9668  


#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -1.305e+00  8.060e-01  -1.619  0.10534    
#Gender            -2.591e-01  2.991e-01  -0.866  0.38634    
#Married            7.798e-01  2.688e-01   2.901  0.00372 ** 
#Dependents        -6.123e-03  1.228e-01  -0.050  0.96024    
#Education          3.530e-01  2.743e-01   1.287  0.19820    
#Self_Employed      4.106e-01  3.692e-01   1.112  0.26610    
#ApplicantIncome    1.823e-05  7.133e-05   0.256  0.79832    
#CoapplicantIncome  1.079e-04  8.721e-05   1.237  0.21620    
#LoanAmount        -5.064e-03  3.053e-03  -1.659  0.09709 .  
#Loan_Amount_Term   6.152e-06  1.902e-03   0.003  0.99742    
#Credit_History     2.226e+00  2.581e-01   8.624  < 2e-16 ***
#Property_Area1     2.711e-01  2.791e-01   0.971  0.33138    
#Property_Area2     6.399e-01  2.825e-01   2.265  0.02349 *  
#---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
#(Dispersion parameter for binomial family taken to be 1)
  
#Null deviance: 591.70  on 471  degrees of freedom
#Residual deviance: 484.35  on 459  degrees of freedom
#AIC: 510.35
  
#Number of Fisher Scoring iterations: 4
  
#Based on the output of the Logistic regression,only 4 variables are significant while other are insignificant.

#Credit_History is an important factor in deciding whether a client will default or not 
#and this was clearly in tune with the outcome of the model. 
#Whether the customer is married or not is also a significant factor, as far as this data set is concerned.
#Property_Area2 and Loan Amount are also significant factors after the above mentioned two attributes.

#Prediction using Logistics Regressor

# Predicting the Test set results
prob_pred = predict(logistics_classifier, type = 'response', newdata = test_set)
My_pred = ifelse(prob_pred > 0.5, 1, 0)


dim(output)
output <- cbind(test_set, My_pred)

#Confusion Matrix
#estimating the performance of the model

cm = table(ActualValue=test_set$Loan_Status, PredictedValue=prob_pred > 0.5)
cm

#           PredictedValue
#ActualValue FALSE TRUE
#         0    18   23
#         1     9   92

#We can check by building a confusion matrix to display the success rate of 
#our model's predictions on the testing data we created earlier.
#The table function builds the confusion matrix. Going diagonally, (18, 92) represent the number of correct predictions. 
#Conversely, the going up diagonally, (9, 23) represent the number of incorrect predictions.

#Estimating the percentage of performance
sum(diag(cm))/sum(cm)
#[1] 0.7746479


#Logistics Regression was able to give us an accuracy of 77.46%, 
#which means that we can expect our model to classify correct about 8 observations in every 10.

