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

(B2HFT <- lm(height~femur + tibia, data = Bones2))

summary(B2HFT)


plot_ly(data=Bones2, z = ~height, x = ~femur, y = ~tibia, opacity = 0.5) %>%
  add_markers()

Fitft <- lm(height ~ femur + tibia, data = Bones2)

x <- seq(380, 550, by = 10)   
y <- seq(300,500,by = 10)
plane <- outer(x,y,function(a,b){Fitft$coefficients[1]+Fitft$coefficients[2]*a+Fitft$coefficients[3]*b})
plot_ly(data=Bones2, z = ~height, x = ~femur, y = ~tibia, opacity = 0.5) %>%
  add_markers() %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)


#4a. List any outliers (#'s bigger than 2)
(zscores <- rstandard(Fitft))
sort(zscores)
absolutezscores <- abs(zscores)
sort(absolutezscores)

#4b.List high Leverage points
(leverage <- hatvalues(Fitft) )
sort(leverage)

highleverage <- function(Fitft) 
  
  cf <- length(coefficients(Fitft))
ff<- length(fitted(Fitft))
ratio <- cf/ff

plot(hatvalues(Fitft), main="Index Plot of Ratio")
abline(h=c(2,3)*ratio, col="red", lty=2)

identify(1:n, hatvalues(Fitft), names(hatvalues(Fitft)))

highleverage(Fitft)


#4c.Influential points (Leverage & Outliers)

(Influential <- cooks.distance(Fitft))
sort(Influential)
