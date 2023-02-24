MidtermData <- read.csv("~/Library/CloudStorage/OneDrive-EasternConnecticutStateUniversity/3rd Year/Spring 2021/MAT 343 Data Analytics/Midterm Project/MidtermData.csv", stringsAsFactors=TRUE)

#First, I'm going to upload the packages I'm going to need for this project
library(ggplot2)
library(caret)
library(MASS)
library(ggcorrplot)
library(ISLR)
library(ROCR)
library(rpart)
library(rpart.plot)
#Now I'm going to get a summary of my data so I know what I'm working with
summary(MidtermData)
#I have a lot of N/A values
#I'm going to see if any variables are closely correlated so I can make a regression model to predict some N/A Values
(corr2 <- cor(MidtermData[,3:13], use = "pairwise.complete.obs", method = "pearson"))
ggcorrplot(corr2, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = FALSE, title="Correlogram of Tuition Variables")
#Since HS Rank and GPA are closesly correlated, I'm going to create a linear regression model to predict some of HS Ranks N/a values
a <- lm(MidtermData$HSRank~MidtermData$HSGPA)

summary(a)

for(i in 1:length(MidtermData$HSRank)){
  if (is.na(MidtermData$HSRank[i]))MidtermData$HSRank[i] <- 34.1617*MidtermData$HSGPA[i]-45.1164
}
summary(MidtermData)
#I was able to predict a ton of N/A values for HS Rank but, i still have 21 N/A Values...
#I'm going to do the same to HS GPA so that they can have the same # of N/A Values
b <- lm(MidtermData$HSGPA~MidtermData$HSRank)

summary(b)

for(i in 1:length(MidtermData$HSGPA)){
  if (is.na(MidtermData$HSGPA[i]))MidtermData$HSGPA[i] <- 0.0205898*MidtermData$HSRank[i]+1.8402023
}
summary(MidtermData)
#I'm going to run another correlation test to see if replacing some of these N/A Values with the mean influenced any other correlations
(corr2 <- cor(MidtermData[,3:13], use = "pairwise.complete.obs", method = "pearson"))
ggcorrplot(corr2, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = FALSE, title="Correlogram of Tuition Variables")

#Now, I'm going to replace the other N/A of the continous variables values with there own means
hist(MidtermData$VSAT)
hist(MidtermData$MSAT)
hist(MidtermData$WSAT)

MidtermData$VSAT[is.na(MidtermData$VSAT)]<-mean(MidtermData$VSAT,na.rm=TRUE)
MidtermData$MSAT[is.na(MidtermData$MSAT)]<-mean(MidtermData$MSAT,na.rm=TRUE)
MidtermData$WSAT[is.na(MidtermData$WSAT)]<-mean(MidtermData$WSAT,na.rm=TRUE)
MidtermData$HSRank[is.na(MidtermData$HSRank)]<-mean(MidtermData$HSRank,na.rm=TRUE)
MidtermData$HSGPA[is.na(MidtermData$HSGPA)]<-mean(MidtermData$HSGPA,na.rm=TRUE)
summary(MidtermData)

#Now I'm going to convert my ERG into a categorical variables because its not supposed to be continuous
MidtermData$ERG <- as.factor(MidtermData$ERG)

#To remove my N/A values, I'm going to distribute them proportionally
summary(MidtermData$ERG)
dummyb<- sample(c("1","2","3","4","5","6","7","8","9"), size = 304, prob = c(133, 604, 533,  618,  208,  487,  174,  508,  316)/3581,replace = TRUE) 
MidtermData$ERG[is.na(MidtermData$ERG)] <- dummyb
summary(MidtermData$ERG)

#I'm going to replace my N/A values for East.West.Out the same way
summary(MidtermData$East.West.Out)
dummyc<- sample(c("east","Out","west"), size = 62, prob = c(1569, 254, 2000)/3823,replace = TRUE) 
MidtermData$East.West.Out[is.na(MidtermData$East.West.Out)] <- dummyc
summary(MidtermData$East.West.Out)

#Now I'm going to make sure that R reads my Categorical Variables correctly 
MidtermData$Athletics <- factor(MidtermData$Athletics ,levels = c(0,1),labels = c("Non-Athletes","Athletes"))
MidtermData$Housing <- factor(MidtermData$Housing ,levels = c(0,1),labels = c("Commuter","Lives On Campus"))
MidtermData$FGEN <- factor(MidtermData$FGEN ,levels = c(0,1),labels = c("Not First Gen","First Gen"))
MidtermData$Stem <- factor(MidtermData$Stem ,levels = c(0,1),labels = c("Not Interested in Stem","Interested in Stem"))
MidtermData$GotSchol <- factor(MidtermData$GotSchol ,levels = c(0,1),labels = c("Recieved No Scholarship","Recieved a Scholarship"))
summary(MidtermData)

#Now I'm going create multiple ERG predictor variables because the variable is not binomial
#When I create this I will be able to extract it from my regression model that AIC Makes
MidtermData$ERG1 <- as.factor(ifelse(MidtermData$ERG == "1" , 1,0))
MidtermData$ERG2 <- as.factor(ifelse(MidtermData$ERG == "2" , 1,0))
MidtermData$ERG3 <- as.factor(ifelse(MidtermData$ERG == "3" , 1,0))
MidtermData$ERG4 <- as.factor(ifelse(MidtermData$ERG == "4" , 1,0))
MidtermData$ERG5 <- as.factor(ifelse(MidtermData$ERG == "5" , 1,0))
MidtermData$ERG6 <- as.factor(ifelse(MidtermData$ERG == "6" , 1,0))
MidtermData$ERG7 <- as.factor(ifelse(MidtermData$ERG == "7" , 1,0))
MidtermData$ERG8 <- as.factor(ifelse(MidtermData$ERG == "8" , 1,0))

#I will do the same with East.West.Out because there are 3 possible values and it is not binomial (binomail values means that they either are significant (1) or they re not (0) )
MidtermData$East <- as.factor(ifelse(MidtermData$East.West.Out == "east" , 1,0))
MidtermData$West <- as.factor(ifelse(MidtermData$East.West.Out == "west" , 1,0))



#Now That I've handled all my missing values, I can begin to partition into a training and testing group so that I can have 'foreign' data to see how accurate my model is
#I will also validate my data for categorical and numerical values
#Before my partition I will set a seed so that every time i run this partition, my data remains consistent

set.seed(1717)

InTrain <- createDataPartition(y = MidtermData$X1st_yr_GPA, p = .75, list = FALSE)


MidTermDataTrain <- MidtermData[InTrain,]
MidTermDataTest <- MidtermData[-InTrain,]

#validating chisquare test(categorical variable)
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$Gender)),as.vector(table(MidTermDataTest$Gender)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$East.West.Out)),as.vector(table(MidTermDataTest$East.West.Out)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$Athletics)),as.vector(table(MidTermDataTest$Athletics)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$Housing)),as.vector(table(MidTermDataTest$Housing)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$FGEN)),as.vector(table(MidTermDataTest$FGEN)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$Stem)),as.vector(table(MidTermDataTest$Stem)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG)),as.vector(table(MidTermDataTest$ERG)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$GotSchol)),as.vector(table(MidTermDataTest$GotSchol)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$Retained)),as.vector(table(MidTermDataTest$Retained)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG1)),as.vector(table(MidTermDataTest$ERG1)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG2)),as.vector(table(MidTermDataTest$ERG2)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG3)),as.vector(table(MidTermDataTest$ERG3)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG4)),as.vector(table(MidTermDataTest$ERG4)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG5)),as.vector(table(MidTermDataTest$ERG5)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG6)),as.vector(table(MidTermDataTest$ERG6)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG7)),as.vector(table(MidTermDataTest$ERG7)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$ERG8)),as.vector(table(MidTermDataTest$ERG8)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$West)),as.vector(table(MidTermDataTest$West)))))$p.value
chisq.test(as.table(rbind(as.vector(table(MidTermDataTrain$East)),as.vector(table(MidTermDataTest$East)))))$p.value


#Validating using kruskal-Wallis (numeric Variables)
MidTermDataTrain$trainortest <- rep("train", nrow(MidTermDataTrain))
MidTermDataTest$trainortest <- rep("test", nrow(MidTermDataTest))
All_Data <- rbind(MidTermDataTrain, MidTermDataTest)
summary(All_Data)

par(mfrow=c(2,3))
b1 <- boxplot(VSAT ~ as.factor(trainortest),data = All_Data)
b2 <- boxplot(WSAT ~ as.factor(trainortest),data = All_Data)
b3 <- boxplot(MSAT ~ as.factor(trainortest),data = All_Data)
b4 <- boxplot(HSGPA ~ as.factor(trainortest),data = All_Data)
b5 <- boxplot(X1st_yr_GPA ~ as.factor(trainortest),data = All_Data)
b6 <- boxplot(HSRank ~ as.factor(trainortest),data = All_Data)


kruskal.test(VSAT ~ as.factor(trainortest),data = All_Data)$p.value
kruskal.test(WSAT ~ as.factor(trainortest),data = All_Data)$p.value
kruskal.test(MSAT ~ as.factor(trainortest),data = All_Data)$p.value
kruskal.test(HSGPA ~ as.factor(trainortest),data = All_Data)$p.value
kruskal.test(HSRank ~ as.factor(trainortest),data = All_Data)$p.value
kruskal.test(X1st_yr_GPA ~ as.factor(trainortest),data = All_Data)$p.value

#Now that I've Partitioned and validated my data, I will let the AIC command pick out the variables by forward, backward, or step-wise selection
#this will be a multiple regression model for my 1st year gpa.
#I will audit the models that the my forward/backward/stepwise made and pick the one with the smallest AIC, or the one I see best fits my model
#Then I will personally see if I should add anything else

head(MidTermDataTrain,1)
Midterm_empty <- lm(X1st_yr_GPA ~ 1,data = MidTermDataTrain)
Midterm_empty
full <- lm(X1st_yr_GPA ~ VSAT+ MSAT + WSAT+ HSRank + East + West + Athletics + Housing + FGEN +  HSGPA + Stem + ERG1+ ERG2 + ERG3 + ERG4 +ERG5 + ERG6 + ERG7 + ERG8+ GotSchol, data = MidTermDataTrain)

M_Forward <- stepAIC(Midterm_empty, direction = "forward", trace = TRUE, scope = formula(full))
#AIC = -163.49
summary(M_Forward)
#R2 adj = 0.1923

M_Backwards <- stepAIC(full, direction = "backward", trace = TRUE)
#AIC = -167.13
summary(M_Backwards)
#R2 adjust = .1938

M_Both2 <- stepAIC(full, direction = "both", trace = TRUE)
#AIC = -167.13
summary(M_Both2)
#R2 adj = .1938

M_Both <- stepAIC(Midterm_empty, direction = "both", trace = TRUE, scope = formula(full))
#AIC = -163.49
summary(M_Both)
#R2 Adj = .1923

#I'm going to pick my Backwards selection for my linear model because its returns the lowest AIC and highest adjusted R^2.

#Now I will create my  multiple linear regression  Model and compare it to testing data to see the accuracy
M_Train <- lm(formula = X1st_yr_GPA ~ MSAT + WSAT + HSRank + East + Housing + 
           FGEN + HSGPA + Stem + ERG1 + ERG2 + ERG3 + ERG4 + ERG5 + 
           ERG7 + ERG8, data = MidTermDataTrain)
summary(M_Train)
#Now that I saw a summary of my coefficients, I'm going to remove my insigifant variables one by one starting with the one with the highest p value
M_Train2 <- lm(formula = X1st_yr_GPA ~ MSAT + WSAT + HSRank + East + Housing + 
                FGEN + HSGPA + Stem + ERG1 + ERG2 + ERG3 + ERG4 + ERG5 + 
                ERG8, data = MidTermDataTrain)
summary(M_Train2)
#ERG 8 now becomes significant but, MSAT is still insignifcant, I will remove this one now.
M_Train3 <- lm(formula = X1st_yr_GPA ~ WSAT + HSRank + East + Housing + 
                 FGEN + HSGPA + Stem + ERG1 + ERG2 + ERG3 + ERG4 + ERG5 + 
                 ERG8, data = MidTermDataTrain)
summary(M_Train3)
#Now that all my variables are significant, I will compare the accuracy of my model compared to the testing data.
M_Train4 <- lm(formula = X1st_yr_GPA ~ WSAT + HSRank + East + Housing + 
                 FGEN + HSGPA + Stem + ERG1 + ERG2 + ERG3 + ERG4 + 
                 ERG8, data = MidTermDataTrain)
summary(M_Train4)

M_Train5 <- lm(formula = X1st_yr_GPA ~ WSAT + HSRank + East + Housing + 
                 FGEN + HSGPA + Stem + ERG1 + ERG3 + ERG4 + 
                 ERG8, data = MidTermDataTrain)
summary(M_Train5)
M_Train6 <- lm(formula = X1st_yr_GPA ~ WSAT + HSRank + East + Housing + 
                 FGEN + HSGPA + Stem + ERG3 + ERG4 + 
                 ERG8, data = MidTermDataTrain)
summary(M_Train6)
M_Train7 <- lm(formula = X1st_yr_GPA ~ WSAT + HSRank + East + Housing + 
                 FGEN + HSGPA + Stem + 
                 ERG8, data = MidTermDataTrain)
summary(M_Train7)

M_Train8 <- lm(formula = X1st_yr_GPA ~ WSAT + HSRank + East + Housing + 
                 FGEN + HSGPA + Stem + 
                 ERG8, data = MidTermDataTrain)
summary(M_Train8)

M_Train9 <- lm(formula = X1st_yr_GPA ~ WSAT + East + Housing + 
                 FGEN + HSGPA + Stem + ERG8, data = MidTermDataTrain)
summary(M_Train9)
M_Train10 <- lm(formula = X1st_yr_GPA ~ East + Housing + 
                 FGEN + HSGPA + Stem + ERG8, data = MidTermDataTrain)
summary(M_Train10)



pr <- predict(M_Train9, newdata = MidTermDataTest)
actual.pred <- data.frame(cbind(actuals = MidTermDataTest$X1st_yr_GPA, predicteds = pr))
#I will use minmax to see the accuracy of my multiple regression model.
(minmax <- mean(apply(actual.pred,1,min)/apply(actual.pred,1,max)))
mape <- mean(abs((actual.pred$predicteds - actual.pred$actuals))/actual.pred$actuals)
mape

#My multiple regression model for first year GPA is 77% Accurate. It will correctly predict 1st year GPA's about 77% of the time.





#Now I will create a retention CART Model to see how 

names(MidTermDataTrain)
MidTermDataTrain <- MidTermDataTrain[,-26]
names(MidTermDataTrain)
names(MidTermDataTest)
MidTermDataTest <- MidTermDataTest[,-26]
names(MidTermDataTest)

Train_Cart <- rpart(Retained ~., data = MidTermDataTrain, method = "class")
rpart.plot(Train_Cart,type = 2, extra = 103)

Pred_Train_Cart <- predict(object = Train_Cart, newdata = MidTermDataTrain, type = "class")
table(MidTermDataTrain$Retained, Pred_Train_Cart)
dim(MidTermDataTrain)[1]
(298+2149)/2916 

(0.8391632 - .75)/0.75


#Testing Cart Model baseline & Accuracy
Test_Cart <- rpart(Retained ~., data = MidTermDataTest, method = "class")
rpart.plot(Test_Cart,type = 2, extra = 104)
#Baseline Model (1st Node)
#76%

Pred_Test_Cart <- predict(object = Test_Cart, newdata = MidTermDataTest, type = "class")
table(MidTermDataTest$Retained, Pred_Test_Cart)

86+729 # = 15186

dim(MidTermDataTest)[1] # = 17109

815/969 # =  0.887603

(0.8410733 -.76)/.76

#K-Fold Cross Validation
Train_Control <- trainControl(method = "cv", number = 100)

Train_Data_CCA <- na.omit(MidTermDataTrain)
model <- train(Retained ~. , data = Train_Data_CCA, method = "rpart", trControl = Train_Control)
print(model)




plot(model$finalModel)

text(model$finalModel)
library(rattle)
#Use Package Rattle to plot the final model
fancyRpartPlot(model$finalModel, cex = 1, main = "Retention Model")



#Now lets look at how it performs:

pred_Kfold_model <- rpart.predict(object = model, newdata = MidTermDataTrain)
table(MidTermDataTrain$Retained,pred_Kfold_model)

# Check for accuracy:

312+2143 # = 2455
dim(MidTermDataTrain)[1] #2916

2455/2916 # 0.8419067

#how it outperformed the baseline model
(0.8419067-.75)/.75

#Retention Model Accuracy

(312+2143)/2916

#Error rate
1- (312+2143)/2916

#Sensitivity

2143/(47+2143)

#specificity

312/(312+414)

#False Positive Rate
1- 312/(312+414)

#False Negative Rate
1- 2143/(47+2143)


#Proportion of True Positive
2134/(2143+414)

#Proportion of True Negative
312/(312+47)

#Proportion of False Positive 
1- 2134/(2143+414)

#Proportion of False Negative
1- 312/(312+47)

#Academic risk model


MidtermData$AtRisk <- as.factor(ifelse(MidtermData$X1st_yr_GPA < 1.700001, "At Academic Risk", "Not At Academic Risk"))
MidTermDataTrain$AtRisk <- as.factor(ifelse(MidTermDataTrain$X1st_yr_GPA < 1.700001, "At Academic Risk", "Not At Academic Risk"))
MidTermDataTest$AtRisk <- as.factor(ifelse(MidTermDataTest$X1st_yr_GPA < 1.700001, "At Academic Risk", "Not At Academic Risk"))

AcademicRiskset <- MidtermData
Academicrisksettrain <- MidTermDataTrain
AcademicRisksettest <- MidTermDataTest



Midterm_emptyAR <- glm(AtRisk ~ 1,data = MidTermDataTrain, family = binomial)
Midterm_emptyAR
fullAR <- glm(AtRisk ~ VSAT+ MSAT + WSAT+ HSRank + East + West + Athletics + Housing + FGEN +  HSGPA + Stem + ERG1+ ERG2 + ERG3 + ERG4 +ERG5 + ERG6 + ERG7 + ERG8+ GotSchol, data = MidTermDataTrain, family = binomial)


# use stepwise so R can make your logistic model

M_ForwardAR <- stepAIC(Midterm_emptyAR, direction = "forward", trace = TRUE, scope = formula(fullAR))
#AIC = 2151.73
summary(M_ForwardAR)
1-with(M_ForwardAR,deviance/null.deviance)

M_BackwardsAR <- stepAIC(fullAR, direction = "backward", trace = TRUE)
#AIC = 2152.24
summary(M_BackwardsAR)
1-with(M_BackwardsAR,deviance/null.deviance)

M_Both2AR <- stepAIC(fullAR, direction = "both", trace = TRUE)
#AIC = 2152.24
summary(M_Both2AR)
1-with(M_Both2AR,deviance/null.deviance)


M_BothAR <- stepAIC(Midterm_emptyAR, direction = "both", trace = TRUE, scope = formula(fullAR))
#AIC = 2151.73
summary(M_BothAR)
1-with(M_BothAR,deviance/null.deviance)

#I'm going to pick my Forward selection for my linear model because its returns the lowest AIC 
#Now I will create my  multiple logistic regression  Model and compare it to testing data to see the accuracy
M_TrainAR <- glm(formula = AtRisk ~ HSGPA + Housing + ERG8 + Stem + ERG5 + 
                   East + ERG1 + FGEN, family = binomial, data = MidTermDataTrain)

summary(M_TrainAR)
#Now that I saw a summary of my coefficients, I'm going to remove my insignificant variables one by one starting with the one with the highest p value
M_Train2AR <- glm(formula = AtRisk ~ HSGPA + Housing + ERG8 + Stem + ERG5 + 
                  East, family = binomial, data = MidTermDataTrain)
summary(M_Train2AR)
#ERG 8 now becomes significant but, MSAT is still insignifcant, I will remove this one now.
M_Train3AR <- glm(formula = AtRisk ~ HSGPA + Housing + ERG8 + Stem + 
                    East, family = binomial, data = MidTermDataTrain)
summary(M_Train3AR)

#Now that all my variables are significant, I will compare the accuracy of my model compared to the testing data.

prAR <- predict(M_Train3AR, newdata = MidTermDataTest)
actual.predAR <- data.frame(cbind(actuals = MidTermDataTest$AtRisk, predicteds = prAR))
#I will use minmax to see the accuracy of my multiple regression model.
(minmaxAR <- mean(apply(actual.predAR,1,min)/apply(actual.predAR,1,max)))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(M_Train3AR))





