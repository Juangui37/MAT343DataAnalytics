summary(Chdata)

#Turn Churn & Voicemail Plan into indicator variables (dummy Variables)
Chdata$Vmail.Plan_Ind <- ifelse(Chdata$VMail.Plan == 'yes', 1, 0)
Chdata$Churn_Ind <- ifelse(Chdata$Churn == 'TRUE', 1, 0)

#Make a table for customers with voicemail plans that churn
(VMChurn <- table(Chdata$VMail.Plan,Chdata$Churn))
#get the proportions
prop.table(VMChurn,1)

#What proportion of customers no voicemail plan churned?
403/ (2008+403)


#probability a customer with a voicemail plan churns
80/(80+842)

#odds he churns
0.0867679/ (1 - 0.0867679)
#log odds he churns
log(0.09501188)

#probability a customer no voicemail plan churns
403/(2008+403)

#odds he churns
0.1671506/ (1- 0.1671506)

#log odd he churns
log(0.2006973)


#find b1 
-2.353753 +1.605957 

#slope of your logistic regression model. Note: it is the difference between the log odds (for Churn) for those customers who have the voicemail and the log odds for customers who do not have voicemail plan. 
#-1.605957 + -0.747796x

#Find the value the slope converted into odds and interpret the infromation
exp(-0.747796)
#Customers that have voicemails are about 0.4734 times as likely to churn compared to customers who don’t have voicemail.

#Using R to do what we just did

Churn_VMail_LR <- glm(Churn_Ind~Vmail.Plan_Ind, data = Chdata, family = binomial )
Churn_VMail_LR


plot(Chdata$Churn_Ind~Chdata$Vmail.Plan_Ind, pch = 16, col = "blue", xlab = "Voicemail Plan", ylab = "Churners (1 = Yes, 0 = No)", main = "Voicemail Plan vs. Churners")
curve(predict(Churn_VMail_LR, data.frame(Vmail.Plan_Ind = x), type = "resp"), add = TRUE, lwd = 2)


Churn_Day.Mins_LR <- glm(Churn_Ind~Day.Mins, data = Chdata, family = binomial )
Churn_Day.Mins_LR

#Converting Log odds to probability for this function
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(Churn_Day.Mins_LR))

logit2prob(coef(Churn_VMail_DayMins_LR))

exp(0.01127)


exp(0.01127)^10

summary(Churn_Day.Mins_LR)

confint(Churn_Day.Mins_LR, level = .95)

exp(confint(Churn_Day.Mins_LR, level = .95))

plot(Chdata$Churn_Ind~Chdata$Day.Mins, pch = 16, col = "blue", xlab = "Day Minutes", ylab = "Churners (1 = Yes, 0 = No)", main = "Day Minutes vs. Churners")
curve(predict(Churn_Day.Mins_LR, data.frame(Day.Mins = x), type = "resp"), add = TRUE, lwd = 2)

#This can be thought of like R^2
1-with(Churn_Day.Mins_LR,deviance/null.deviance)

library(ROCR)
library(caret)
library(MASS)
library(ISLR)

#Multiple Logistic Regression

Churn_VMail_DayMins_LR <- glm(Churn_Ind~Vmail.Plan_Ind + Day.Mins, data = Chdata, family = binomial )
Churn_VMail_DayMins_LR

summary(Churn_VMail_DayMins_LR)

summary(Churn_Day.Mins_LR)
summary(Churn_VMail_LR)

1-with(Churn_Day.Mins_LR,deviance/null.deviance)
1-with(Churn_VMail_DayMins_LR,deviance/null.deviance)
1-with(Churn_VMail_LR,deviance/null.deviance)

(VMDProb <- logit2prob(coef(Churn_VMail_DayMins_LR)))

(pred_bothv_churn <- predict(VMDProb))

(pred_bothv_churn <- predict(Churn_VMail_DayMins_LR, type = "response"))




Churn_VMail_DayMins_LR

new.data1 <- data.frame(Vmail.Plan_Ind = 0, Day.Mins = 175 )
#Logodds (predict1 <- predict(Churn_VMail_DayMins_LR, newdata = new.data1))
(predict1 <- predict(Churn_VMail_DayMins_LR, newdata = new.data1, type = "response"))


new.data2 <- data.frame(Vmail.Plan_Ind = 1, Day.Mins = 175 )
#Logodds (predict1 <- predict(Churn_VMail_DayMins_LR, newdata = new.data2))
(predict2 <- predict(Churn_VMail_DayMins_LR, newdata = new.data2, type = "response"))


new.data3 <- data.frame(Vmail.Plan_Ind = 0, Day.Mins = 300 )
#Logodds (predict1 <- predict(Churn_VMail_DayMins_LR, newdata = new.data3))
(predict3 <- predict(Churn_VMail_DayMins_LR, newdata = new.data3, type = "response"))

new.data4 <- data.frame(Vmail.Plan_Ind = 1, Day.Mins = 300 )
#Logodds (predict1 <- predict(Churn_VMail_DayMins_LR, newdata = new.data4))
(predict4 <- predict(Churn_VMail_DayMins_LR, newdata = new.data4, type = "response"))



