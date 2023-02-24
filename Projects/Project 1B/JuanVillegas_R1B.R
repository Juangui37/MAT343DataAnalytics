library(caret)
library(ggplot2)

summary(Bones)
#There are some N/A's So I'm going to create a table with the 2 variables I'm using
#& I'm going to remove the N/As

Comp <- complete.cases(Bones)
summary(Comp)


height <- Bones$height[Comp==TRUE]
fibula <- Bones$fibula[Comp==TRUE]

Modbones <- data.frame(height,fibula)
summary(Modbones)
#There's no more N/A's, Now I can make a scatter plot

#1.Make a scatterplot of height vs fibula
plot(Modbones$height~Modbones$fibula)

#2 preform a regression of height on fibula length. 
(M_Modbones <- lm(height~fibula, data = Modbones))
# y(height) = 62.4441 + 0.2905(fibula)

#2d, What % of the variability in height is explained in the LSRL
summary(M_Modbones)
# R^2 = .7007, Variability = 70%


#3A. Create a residual plot (residuals vs fitted value graph)
plot(M_Modbones$residuals~M_Modbones$fitted.values, ylab = "Residuals", xlab = "Fitted Values", main = "Residual Plot")
abline(0,0, col = "red")

#3b. Are residuals normally distributed? Equal variance assumption is reasonable? (Make a Q- Plot)
qqnorm(M_Modbones$residuals)
# Its asking if the Q-Q plot is linear

#3c. Make a hypthosis test (HO: & HA: statements) & reject/accept w p-value
summary(M_Modbones)

#Getting all the graphs
par(mfrow=c(2,2))
plot(M_Modbones)
par(mfrow=c(1,1))

#Determine & Interpret 95% confidence interal for slope
confint(M_Modbones, level = .95)



#4a. List any outliers (#'s bigger than 2)
(zscores <- rstandard(M_Modbones))
sort(zscores)
absolutezscores <- abs(zscores)
sort(absolutezscores)

#4b.List high Leverage points
(leverage <- hatvalues(M_Modbones) )
sort(leverage)

#4c.Influential points (Leverage & Outliers)

(Influential <- cooks.distance(M_Modbones))
sort(Influential)


#5a.confidence interval for all people whos fibula measures 350mm. 

new.fib <- data.frame(fibula = 350)

(pred_350 <- predict(M_Modbones, newdata = new.fib))

(CI_350 <- predict(M_Modbones, newdata = new.fib, interval = "confidence"))

#5b Prediction interval for someone (one person) whos fibula measures 350mm
(PI_350 <- predict(M_Modbones, newdata = new.fib, interval = "prediction"))


#5c. GGplot scatter plot with confidence interval bands and 95% prediction interval band 


predict.int <- predict(M_Modbones, interval = "prediction")
mydata <- cbind(Modbones,predict.int)
summary(mydata)
head(mydata)
(Modbones_SP <- ggplot(mydata,aes(x=fibula, y = height))+
    geom_point(color = "black", size = 1.5)+
    geom_smooth(method = "lm", color = "blue") + geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y=upr), color = "red", linetype = "dashed"))

anova(M_Modbones)


#6a.Partitioning Bone Data
set.seed(67)

inTrain <- createDataPartition(y=mydata$height, p =.7, list = FALSE)

mydata.train <- mydata[inTrain,]
mydata.test <- mydata[-inTrain,]

summary(mydata.train)
summary(mydata.test)

mydata.train$trainortest <- rep("train", nrow(mydata.train))
mydata.test$trainortest <- rep("test", nrow(mydata.test))

mydata.all <- rbind(mydata.train, mydata.test)

summary(mydata.all)


boxplot(height~as.factor(trainortest), data = mydata.all, horizontal = TRUE, col = "blue")
boxplot(fibula~as.factor(trainortest), data = mydata.all, horizontal = TRUE, col = "blue")

kruskal.test(fibula~as.factor(trainortest), data = mydata.all)$p.value
kruskal.test(height~as.factor(trainortest), data = mydata.all)$p.value

#7a. preform a regression of height on fibula length using the training dataset
(M_train <- lm(height~fibula, data = mydata.train))
summary(M_train)

#Predict the heights for the fibula lengths in your testing dataset. 

HeightPred <- predict(M_train,newdata = mydata.test)
actuals_preds <- data.frame(cbind(actuals = mydata.test$fibula,predicteds=HeightPred))
head(actuals_preds)

#calculate min max accuracy and mean absolute percent error.

min_max_accuracy <- mean(apply(actuals_preds,1,min)/apply(actuals_preds,1,max))
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

#7d. predict the height of a deceased person with a fibula length of 387mm
new.fib1 <- data.frame(fibula = 387)

(pred_387 <- predict(M_Modbones, newdata = new.fib1))

(PI_350 <- predict(M_Modbones, newdata = new.fib1, interval = "prediction"))
