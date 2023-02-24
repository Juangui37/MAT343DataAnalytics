summary(tuition)
natuiton <- na.omit(tuition)
summary(natuiton)


library(ggplot2)
library(gridExtra)
library(MASS)

#Data Partitioning
library(caret)
set.seed(117)

InTrain <- createDataPartition(y = natuiton$tuition, p = .80, list = FALSE)


tuitiontrain <- natuiton[InTrain,]

tuitiontest <- natuiton[-InTrain,]


tuitiontrain$trainortest <- rep("train", nrow(tuitiontrain))
tuitiontest$trainortest <- rep("test", nrow(tuitiontest))
All_Data <- rbind(tuitiontrain, tuitiontest)

summary(All_Data)

library(ggcorrplot)

(corr <- cor(All_Data[-12], use = "pairwise.complete.obs", method = "pearson"))

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = FALSE, title="Correlogram of Tuition Variables")

library(ggpubr)


(a <- ggscatter(All_Data, x = "pcttop25", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Students in the 25% of there HS class ", ylab = "Tuition rate"))

qplot(pcttop25, tuition, data = All_Data)

(pcctop25 <- qqnorm(All_Data$tuition, All_Data$pcctop25))
LogPcctop25 <- log10(a)
Logpccto

(b <- ggscatter(All_Data, x = "sf_ratio", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Student To Faculty Ratio", ylab = "Tuition rate"))

qplot(sf_ratio, tuition, data = All_Data)

qqplot(All_Data$tuition, All_Data$sf_ratio)

(c <- ggscatter(All_Data, x = "fac_comp", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Average Faculty Compensation", ylab = "Tuition rate"))

qqplot(All_Data$tuition, All_Data$fac_comp)

(d <- ggscatter(All_Data, x = "accrate", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Fraction of applicants accepted into admisions", ylab = "Tuition rate"))

qqplot(All_Data$tuition, All_Data$accrate)


e <- ggscatter(All_Data, x = "graduat", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of students who graduate", ylab = "Tuition rate")

qqplot(All_Data$tuition, All_Data$graduat)

f <- ggscatter(All_Data, x = "pct_phd", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of faculty with PHD", ylab = "Tuition rate")

qqplot(All_Data$tuition, All_Data$pct_phd)


g <- ggscatter(All_Data, x = "fulltime", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of undergraduates who are fulltime", ylab = "Tuition rate")

qqplot(All_Data$tuition, All_Data$fulltime)

h <- ggscatter(All_Data, x = "alumni", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of of alumni who graduate", ylab = "Tuition rate")

qqplot(All_Data$tuition, All_Data$alumni)

(i <- ggscatter(All_Data, x = "num_enrl", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "number of new students enrolled", ylab = "Tuition rate"))

qplot(num_enrl, tuition, data = All_Data)

qplot(num_enrl, tuition, data = All_Data) + scale_y_continuous(trans = 'x^2')

qqplot(All_Data$tuition, All_Data$num_enrl)

j <- ggscatter(All_Data, x = "public.private", y = "tuition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Is the College Public or Private", ylab = "Tuition rate")

qqplot(All_Data$tuition, All_Data$public.private)

library(gridExtra)

#Use The dataset with omitted values

head(natuiton)



#Forward Selection step by step
tuition_empty <- lm(tuition ~ 1,data = tuitiontrain)

tuition_empty
(StepF1 <- add1(tuition_empty, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))
tuition_empty1 <- lm(tuition ~ public.private,data = tuitiontrain)
summary(tuition_empty1)
(StepF2 <- add1(tuition_empty1, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))
tuition_empty2 <- lm(tuition ~ public.private+ fac_comp,data = tuitiontrain)
summary(tuition_empty2)
(StepF3 <- add1(tuition_empty2, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))

tuition_empty3 <- lm(tuition ~ public.private+ fac_comp + alumni ,data = tuitiontrain)

summary(tuition_empty3)

(StepF4 <- add1(tuition_empty3, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))

tuition_empty4 <- lm(tuition ~ public.private+ fac_comp + alumni +sf_ratio ,data = tuitiontrain)

summary(tuition_empty4)

(StepF5 <- add1(tuition_empty4, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))

tuition_empty5 <- lm(tuition ~ public.private+ fac_comp + alumni +sf_ratio +graduat ,data = tuitiontrain)

summary(tuition_empty5)

(StepF6 <- add1(tuition_empty5, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))

tuition_empty6 <- lm(tuition ~ public.private+ fac_comp + alumni +sf_ratio +graduat + pct_phd ,data = tuitiontrain)

summary(tuition_empty6)

(StepF7 <- add1(tuition_empty6, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))

tuition_empty7 <- lm(tuition ~ public.private+ fac_comp + alumni +sf_ratio +graduat+pct_phd + num_enrl ,data = tuitiontrain)

summary(tuition_empty7)

(StepF8 <- add1(tuition_empty7, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))

tuition_empty8 <- lm(tuition ~ public.private+ fac_comp + alumni +sf_ratio +graduat +pct_phd + num_enrl + fulltime ,data = tuitiontrain)

summary(tuition_empty8)

(StepF9 <- add1(tuition_empty8, scope = tuitiontrain[,1:11], test = "F", trace = TRUE))
summary(tuition_empty8)

# Now well do Forward, backwards & Step wise forward & backward using AIC

head(tuitiontrain)
full <- lm(tuition ~ pcttop25 + sf_ratio + fac_comp+ accrate + graduat+ pct_phd + fulltime+ alumni + num_enrl + public.private, data = tuitiontrain)

M_Forward <- stepAIC(tuition_empty, direction = "forward", trace = TRUE, scope = formula(full))

M_Backwards <- stepAIC(full, direction = "backward", trace = TRUE)

M_Both2 <- stepAIC(full, direction = "both", trace = TRUE)

M_Both <- stepAIC(tuition_empty, direction = "both", trace = TRUE, scope = formula(full))


summary(M_Backwards)
summary(M_Forward)
summary(M_Both)


# Fill N/A's With mean
library(zoo)

na1 <- tuition
mod2 <- na.aggregate(na1)

summary(tuition)
summary(mod2)


#Use the filled na with means to make a new data model

tuition_empty2 <- lm(tuition ~ 1,data = mod2)
tuition_empty2


head(natuiton)
full <- lm(tuition ~ pcttop25 + sf_ratio + fac_comp+ accrate + graduat+ pct_phd + fulltime+ alumni + num_enrl + public.private, data = mod2)

M_Forward2 <- stepAIC(tuition_empty2, direction = "forward", trace = TRUE, scope = formula(full))

M_Backwards2 <- stepAIC(full, direction = "backward", trace = TRUE)

M_Both3 <- stepAIC(full, direction = "both", trace = TRUE)

M_Both4 <- stepAIC(tuition_empty2, direction = "both", trace = TRUE, scope = formula(full))


summary(M_Backwards2)
summary(M_Forward2)
summary(M_Both3)

#Select a model from above, apply the model to predict tuition using the explanatory variables in tuitiontest data


M_Train <- lm(formula = tuition ~ public.private + fac_comp + alumni + sf_ratio + 
                graduat + pct_phd + num_enrl + fulltime, data = tuitiontrain) 


summary(M_Train)

#Predict the tuition for the tuition in your testing data 

TuitionPred <- predict(M_Train,newdata = tuitiontest)
actuals_preds <- data.frame(cbind(actuals = tuitiontest$public.private + tuitiontest$pct_phd + tuitiontest$fulltime + tuitiontest$num_enrl+ tuitiontest$graduat+ tuitiontest$fac_comp+tuitiontest$alumni +tuitiontest$sf_ratio,predicteds= TuitionPred))
head(actuals_preds)

#calculate min max accuracy and mean absolute percent error.

min_max_accuracy <- mean(apply(actuals_preds,1,min)/apply(actuals_preds,1,max))
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape


