library(ggplot2)
library(plotly)

cor(Bones$height,Bones$fibula, use = "complete.obs")
summary(Bones)

#Finding the correlation between all the numeric variables. (we took out sex hence the [])
#Reminder: For Bones[,2:8], the blank before the first comma means
#we will use all the records (rows). The 2:8 after the comma indicates that we're including
#the variables in columns 2 - 8. 
#You also could have accomplished this using Bones[,-1].
#This tells R to exclude the first variable. 

cor(Bones[,2:8], use = "pairwise.complete.obs", method = "pearson")
cor(Bones[,-1], use = "pairwise.complete.obs", method = "pearson")

(BonesLM <- lm(height~femur, data = Bones))
summary(BonesLM)


#removing N/A's from femur & tibia
mean(Bones$femur, na.rm = TRUE)
mean(Bones$tibia, na.rm = TRUE)


Bones$femur[is.na(Bones$femur)]<-mean(Bones$femur,na.rm=TRUE)
Bones$tibia[is.na(Bones$tibia)]<-mean(Bones$tibia,na.rm=TRUE)


height <- Bones$height
femur <- Bones$femur
tibia <- Bones$tibia

Bones2 <- data.frame(femur,tibia,height)

(B2 <- lm(height~femur, data = Bones2))

summary(B2)

(M_B2 <- lm(height~femur + tibia, data = Bones2))

summary(B2HFT)


plot_ly(data=Bones2, z = ~height, x = ~femur, y = ~tibia, opacity = 0.5) %>%
  add_markers()

x <- seq(380, 550, by = 10)   
y <- seq(300,500,by = 10)
plane <- outer(x,y,function(a,b){B2HFT$coefficients[1]+B2HFT$coefficients[2]*a+B2HFT$coefficients[3]*b})
plot_ly(data=Bones2, z = ~height, x = ~femur, y = ~tibia, opacity = 0.5) %>%
  add_markers() %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)


#4a. List any outliers (#'s bigger than 2)
(zscores <- rstandard(M_B2))
sort(zscores)
absolutezscores <- abs(zscores)
sort(absolutezscores)

#4b.List high Leverage points
#Considered leverage points after this: 
#3(m+1)/n
# m = predictor variables
# n = # of data values in data set
3*(2+1)/225
(leverage <- hatvalues(M_B2))
sort(leverage)



#Visual representation of high leverage values

cf <- length(coefficients(M_B2))
ff<- length(fitted(M_B2))
ratio <- cf/ff

plot(hatvalues(M_B2), main="Index Plot of Ratio")
abline(h=c(2,3)*ratio, col="red", lty=2)
identify(1:225, hatvalues(Fitft), names(hatvalues(Fitft)))



#4c.Influential points (Leverage & Outliers)


#To be considered an influential point it has to be bigger than 1
(Influential <- cooks.distance(M_B2))
sort(Influential)
#None here


#Way to visually see if theres a cut off

cutoff <- 4/(nrow(height)-length(M_B2$coefficients)-2)
plot(M_B2, which=4, cook.levels=cutoff) 
abline(h=cutoff, lty=2, col="red")

#No red line = No cut off



#Checking to see if the assumptions for inference are satisfied (residual vs fitted plot) (normal QQ plot)
par(mfrow = c(2,2))
plot(M_B2)
par(mfrow = c(1,1))

#Test hypothesis for coefficients
summary(M_B2)

#p-val is very small (less than 0.05) so we're going to reject the null hypothesis of being = 0

#95 confident interval
confint(B2HFT, level = .95)



anova(B2HFT)


#estimate the mean height of people whos femur are 512mm & tibia = 461mm
new.data2 <- data.frame(femur = 512, tibia = 461)
(pred_new <- predict(M_B2, newdata = new.data2))

(CI_new <- predict(M_B2, newdata = new.data2, interval = "confidence"))

#5b Prediction interval for someone (one person) whos fibula measures 350mm
(PI_new <- predict(M_B2, newdata = new.data2, interval = "prediction"))

#Random

predict.int <- predict(M_B2, interval = "prediction")
mydata <- cbind(Bones2,predict.int)
head(mydata)

