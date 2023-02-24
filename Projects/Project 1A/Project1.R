#1 Run summary
summary(bird)
#2a. a make a histogram
hist(bird$Weight, xlab = "Weight",
     ylab = "Amount of birds with this Weight",
     main = "Histogram of Bird Weight", col = "Blue")
#2b. Is there outliers? Boxplot
boxplot(bird$Weight, horizontal = TRUE, xlab = "Weight (g)", col = "coral")

#1c. Make a Quantile Plot
qqnorm(bird$Weight)
qqline(bird$Weight)

#3a. Make a scatterplot of weight vs wing length
plot(bird$Weight~bird$Wing, xlab = "Wing Length (cm)",
     ylab = "Bird Weight (g)", pch = 16, col = "blue")

#3b. Fit a leastsquares regression line
(bird_model <- lm(bird$Weight~bird$Wing))
#3c. Overlay the LSRL on scatterplot
abline(bird_model, col = "red")
summary(bird)

#4a make plot of residuals vs wings...
plot(bird_model$residuals, bird$Wing)
#can't bc length is not the same (N/A values)

#4b. Plot residuals vs fitted values
plot(bird_model$fitted.values , bird_model$residuals, ylab = "Residuals", xlab = "Fitted Values", main = "Graph of Residuals on Fitted Values")
abline(0,0, col = "red")

#4c. run Anova command to regression model: calculate SSR, SSE & SST
anova(bird_model)
#4d. run Summary of regression model. Determine MSE & Se
summary(bird_model)

#5a calculate mean & sd
mean(bird$Weight)
sd(bird$Wing)
#Can't because theres N/A Values

#5b. fix this N/a rpoblem
mean(bird$Weight, na.rm = TRUE)
sd(bird$Weight, na.rm = TRUE)

mean(bird$Wing, na.rm = TRUE)
sd(bird$Wing, na.rm = TRUE)

#5a. Calculate correlation b/w weight & wing length
cor(bird$Weight,bird$Wing)
#can't because NA

#Fix that N/a problem
cor(bird$Weight,bird$Wing, use = "complete.obs")

#6a. see how many rows ahve N/a values
Comp <- complete.cases(bird)
summary(Comp)

# 6b.make new dataframe with no N/a values
weight <- bird$Weight[Comp ==TRUE]
wing <- bird$Wing[Comp ==TRUE]
modbird <- data.frame(wing, weight)
summary(modbird)

#7 Make scatterplot and lsrl with new model.
plot(modbird$weight~modbird$wing, xlab = "Wing Length (cm)",
     ylab = "Bird Weight (g)", pch = 16, col = "blue")
(model2 <- lm(modbird$weight~modbird$wing))

plot(modbird$wing, model2$residuals, xlab = "Wing Size (cm)", ylab = "Residuals", main =  "Wing Size Residuals", pch = 16)
abline(0,0,col = "red")

#7c. plot all the models in one plot image
par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(1,1)) 

#8. use ggplot
library(ggplot2)

ggplot(modbird, aes(x = wing, y = weight))+
        geom_point()+
        geom_smooth(method = "lm")
ggplot(modbird, aes(x = wing, y = weight))+
        geom_point( color = "blue", shape = 2, size = 1)+ 
        geom_smooth(method = "lm", color = "red")
