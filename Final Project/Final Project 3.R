##### Part 3 of Final Project ####
summary(Data)
Data<-Data[!(Data$East.West.Out==""),]
summary(Data$East.West.Out)

#### 1a. Remove NA's ####
Data2 <- na.omit(Data)
summary(Data2)
Data2

#### 1b. replace gender with M =1 & f =2. ####
Data2$Gender <- ifelse(Data2$Gender=="M",1,0)

#### 1c. replace The east.west.out with east =1, west = 1, & f =2. ####
Data2$East <- ifelse(Data2$East.West.Out=="east", 1,0)
Data2$West <- ifelse(Data2$East.West.Out=="west", 1,0)

#### 1d. Create a new variable ERG_low, which classifies schools ranked 5 – 9 as 1 (for low) and 0 (for high). ####
Data2$low <- (ifelse(Data2$ERG > 4, 1, 0))

##### 1e. Remove the variable retained #####
Data2 = subset(Data2, select = -c(Retained) )

##### 1.f Create an at risk varable ####
Data2$AtRisk <- (ifelse(Data2$X1st_yr_GPA < 2.3 , 1, 0))


##### 1.g normalize numeric data #####
#normalize <- function(x) {return((x - min(x))/(max(x) - min(x)))}
#maximin.df <- as.data.frame(lapply(Data2,normalize))
#head(maximin.df)

Data2$VSAT = (Data2$VSAT- min(Data2$VSAT))/ (max(Data2$VSAT)- min(Data2$VSAT))
Data2$MSAT = (Data2$MSAT- min(Data2$MSAT))/ (max(Data2$MSAT)- min(Data2$MSAT))
Data2$WSAT = (Data2$WSAT- min(Data2$WSAT))/ (max(Data2$WSAT)- min(Data2$WSAT))
Data2$HSRank = (Data2$HSRank- min(Data2$HSRank))/ (max(Data2$HSRank)- min(Data2$HSRank))
Data2$HSGPA = (Data2$HSGPA- min(Data2$HSGPA))/ (max(Data2$HSGPA)- min(Data2$HSGPA))
Data2$X1st_yr_GPA = (Data2$X1st_yr_GPA- min(Data2$X1st_yr_GPA))/ (max(Data2$X1st_yr_GPA)- min(Data2$X1st_yr_GPA))

summary(Data2)

##### 2. Partition the data into training and testing data ####
library(caret)

set.seed(1717)
inTrain <- createDataPartition(y=Data2$X1st_yr_GPA,p=.8,list=FALSE)
training <- Data2[inTrain,]
testing <- Data2[-inTrain,]

##### 3a. create a subset of the training data that contains only the variables Gender, VSAT, Housing, HSGPA, X1st_year_GPA ####
names(training)
training2 <- training[ -c(2,4:6,8,9,11:13,15:18)]
testing2 <- testing[ -c(2,4:6,8,9,11:13,15:18)]

names(testing2)

##### 3b. Use the neuralnet package ####
library(neuralnet)

##### 3c. Make a neural network for the target variable (X1st_yr_GPA). Make sure it only has one hidden layer & 2 nodes ####

nnA <- neuralnet(X1st_yr_GPA~.,data = training2, hidden=2)
plot(nnA)

nnA$result.matrix



##### 3e. Next, take your testing2 data frame and use it to test how well your network has done in predicting the "at risk" students. #####

temp_test2 <- testing2[-4]
names(temp_test2)
nnA.results <- compute(nnA,temp_test2)
results2 <- data.frame(actual = testing2$X1st_yr_GPA, prediction=nnA.results$net.result)

roundedresults2 <- sapply(results2,round,digits = 0)

roundedresults2df <- data.frame(roundedresults2)

T2 <- table(roundedresults2df$actual, roundedresults2df$prediction)

dimnames(T2) <- list(actual = c("GPA>=2.3","GPA < 2.3 At Risk"), prediction = c("Not at Risk","At Risk"))

T2

#accuracy 
39+380
dim(roundedresults2df)[1]
419/455

0.9208791*100


100-92.08791






##### 4 ####
library(MASS)
training3 <- training[-c(2,9,12)]
testing3 <- testing[-c(2,9,12)]
training3 <- training3[-c(11)]
testing3 <- testing3[-c(11)]
names(training3)
empty_training <- lm(AtRisk ~ 1,data = training3)
empty_training
full <- lm(AtRisk ~ Gender + VSAT+ MSAT + WSAT + Athletics + Housing + FGEN +  HSGPA + Stem +  GotSchol + East + West + low , data = training3)

M_Forward <- stepAIC(empty_training, direction = "forward", trace = TRUE, scope = formula(full))
#AIC = -3458.99
summary(M_Forward)
#R2 adj = 0.1109

M_Backwards <- stepAIC(full, direction = "backward", trace = TRUE)
#AIC = -3458.99
summary(M_Backwards)
#R2 adjust = 0.1109 

##### Backwards & Forward gave me the same result ####
Model <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East + 
     Housing + low + Athletics, data = training3)
summary(Model)

Model2 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East + 
              Housing + low, data = training3)
summary(Model2)

Model3 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East + 
               Housing, data = training3)
summary(Model3)
Model4 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East, data = training3)
summary(Model4)
Model5 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN, data = training3)
summary(Model5)
Model6 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender, data = training3)
summary(Model6)

nnA2 <- neuralnet(AtRisk ~ HSGPA + Stem + MSAT + Gender, data = training3, hidden=c(4,1))
plot(nnA2)


names(testing3)
temp_test3 <- testing3[-4]
names(temp_test3)
nnA2.results <- compute(nnA2,temp_test3)
results3 <- data.frame(actual = testing3$AtRisk, prediction=nnA2.results$net.result)

roundedresults3 <- sapply(results3,round,digits = 0)

roundedresults3df <- data.frame(roundedresults3)

T3 <- table(roundedresults3df$actual, roundedresults3df$prediction)

dimnames(T3) <- list(actual = c("GPA>=2.3","GPA < 2.3 At Risk"), prediction = c("Not at Risk","At Risk"))

T3

#accuracy 
343+22
dim(roundedresults3df)[1]
365/455

0.8021978*100

100-80.21978


##### 5 ####
Data3 <- na.omit(Data)
summary(Data3)
Data3

#### 1b. replace gender with M =1 & f =2. ####
Data3$Gender <- ifelse(Data3$Gender=="M",1,0)

#### 1c. replace The east.west.out with east =1, west = 1, & f =2. ####
Data3$East <- ifelse(Data3$East.West.Out=="east", 1,0)
Data3$West <- ifelse(Data3$East.West.Out=="west", 1,0)

#### 1d. Create a new variable ERG_low, which classifies schools ranked 5 – 9 as 1 (for low) and 0 (for high). ####
Data3$low <- (ifelse(Data3$ERG > 4, 1, 0))

##### 1.f Create an at risk variable ####
Data3$AtRisk <- (ifelse(Data3$X1st_yr_GPA < 2.3 , 1, 0))
set.seed(177)
inTrain1 <- createDataPartition(y=Data3$Retained,p=.8,list=FALSE)
training1 <- Data3[inTrain1,]
testing1 <- Data3[-inTrain1,]
names(training1)

training10 <- training1[-c(2,9,12)]
testing10 <- testing1[-c(2,9,12)]

names(training10)
empty_training1 <- lm(Retained ~ 1,data = training10)
empty_training1
full1 <- lm(AtRisk ~ Gender + VSAT+ MSAT + WSAT + Athletics + Housing + FGEN +  HSGPA + Stem +  GotSchol + X1st_yr_GPA + East + West + low + AtRisk , data = training10)

M_Forward <- stepAIC(empty_training1, direction = "forward", trace = TRUE, scope = formula(full1))
#AIC = -3458.99
summary(M_Forward)
#R2 adj = 0.1109

M_Backwards <- stepAIC(full, direction = "backward", trace = TRUE)
#AIC = -3458.99
summary(M_Backwards)
#R2 adjust = 0.1109 

##### Backwards & Forward gave me the same result ####
Model <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East + 
              Housing + low + Athletics, data = training3)
summary(Model)

Model2 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East + 
               Housing + low, data = training3)
summary(Model2)

Model3 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East + 
               Housing, data = training3)
summary(Model3)
Model4 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN + East, data = training3)
summary(Model4)
Model5 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender + FGEN, data = training3)
summary(Model5)
Model6 <- lm(formula = AtRisk ~ HSGPA + Stem + MSAT + Gender, data = training3)
summary(Model6)

nnA2 <- neuralnet(AtRisk ~ HSGPA + Stem + MSAT + Gender, data = training3, hidden=c(4,1))
plot(nnA2)


names(testing3)
temp_test3 <- testing3[-4]
names(temp_test3)
nnA2.results <- compute(nnA2,temp_test3)
results3 <- data.frame(actual = testing3$AtRisk, prediction=nnA2.results$net.result)

roundedresults3 <- sapply(results3,round,digits = 0)

roundedresults3df <- data.frame(roundedresults3)

T3 <- table(roundedresults3df$actual, roundedresults3df$prediction)

dimnames(T3) <- list(actual = c("GPA>=2.3","GPA < 2.3 At Risk"), prediction = c("Not at Risk","At Risk"))

T3

#accuracy 
343+22
dim(roundedresults3df)[1]
365/455

0.8021978*100

100-80.21978
