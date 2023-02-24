##### Final Project Part 2 ####
summary(grades1)
#### #2 Plot the dataframe ####
plot(grades1)

##### #3A remove student ID numbers ####
df = subset(grades1, select = -c(1) )
df

##### #3B Make plot of WSS & number of clusters ####
(KM <- kmeans(df, centers = 3, nstart = 25))
KM$tot.withinss


wss <-  numeric(11)
for (k in 1:11) wss[k] <- sum(kmeans(df, centers = k, nstart = 25)$withinss)
wss
plot(1:11, wss, type = "b", pch = 16, col = "blue", xlab = "Number of clusters", ylab = "wss")


(Df.3means <- kmeans(df, centers = 3, nstart = 25))
Df.3means$centers


##### #4a plot the clusters 2 variables at a time ####
#Math versus English
plot(df[c("English","Math")], col = Df.3means$cluster, pch = 16)

#Science versus English
plot(df[c("Science","English")], col = Df.3means$cluster, pch = 16)

#Science versus Math
plot(df[c("Science","Math")], col = Df.3means$cluster, pch = 16)


#Using GGPlot#
library(ggplot2)

df$cluster <- factor(KM$cluster)
centers <- as.data.frame(KM$centers)
#Here's the code for my first plot, Math versus English
(g1 <- ggplot(data=df,aes(x=English,y=Math,color = cluster))+
  geom_point()+
  geom_point(data=centers, 
             aes(x=English,y = Math, color =as.factor(c(1,2,3))),
             size = 10,alpha = 0.3))

(g2 <- ggplot(data=df,aes(x=Science,y=English,color = cluster))+
    geom_point()+
    geom_point(data=centers, 
               aes(x=Science,y = English, color =as.factor(c(1,2,3))),
               size = 10,alpha = 0.3))

(g3 <- ggplot(data=df,aes(x=Science,y=Math,color = cluster))+
    geom_point()+
    geom_point(data=centers, 
               aes(x=Science,y = Math, color =as.factor(c(1,2,3))),
               size = 10,alpha = 0.3))


library(ggpubr)

ggarrange(g1, g2, g3 + rremove("x.text"), 
          labels = c("English VS Math", "Science VS English", "Science VS Math"),
          ncol = 2, nrow = 2)

##### Making it a 3D Graph ####

df$cluster = factor(kmeans(df,3)$cluster)

library(plotly)
library(dplyr)
p <- plot_ly(df, x=~English, y=~Math, 
             z=~Science, color=~cluster) %>%
  add_markers(size=1.5)
print(p)

library(ggcorrplot)
(corr <- cor(df, use = "pairwise.complete.obs", method = "pearson"))
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = TRUE, title="Correlogram of Grade Subject Variables")

