library(caret)
library(ggplot2)

summary(Bones)

Comp <- complete.cases(Bones)
summary(Comp)


height <- Bones$height[Comp==TRUE]
fibula <- Bones$fibula[Comp==TRUE]

(Modbones <- data.frame(height,fibula))
summary(Modbones)
(M_Modbones <- lm(height~fibula, data = Modbones))
par(mfrow=c(2,2))
plot(M_Modbones)
par(mfrow=c(1,1))
summary(M_Modbones)
ggplot(Modbones, aes(x = fibula, y = height))+
  geom_point(color = "red", size = 1)+
  geom_smooth(method = "lm", color = "blue")

(M_Modbones <- lm(height~fibula, data = Modbones))
summary(M_Modbones)

confint(M_Modbones)
confint(M_Modbones, level = .90)





pred.int <- predict(M_Modbones, interval = "prediction")
mydata <- cbind(M_Modbones,pred.int)


(p <- ggplot(mydata,aes(x=fibula, y = height))+
  geom_point(color = "black", size = 3)+
  geom_smooth(method = "lm", color = "blue") + geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed"))
