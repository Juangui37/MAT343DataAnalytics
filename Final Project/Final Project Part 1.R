
#### Begining of Final Project ####

summary(SATData)

#### Number 1: Remove NA Values####
SATDataNA <- na.omit(SATData)

#### 2A. Create a new dataframe with just VSAT & MSAT####
SAT_VM <- SATDataNA[c("VSAT","MSAT")]
summary(SAT_VM)
#### 2B. Plot Math & Verbal Scores (Scatterplot)####
plot(MSAT ~ VSAT, data = SAT_VM)


#### 3A. Separate the SAT scores into three clusters. Show the output of the Centers. ####

#### Finding K value ####
wss <-  numeric(11)
for (k in 1:11) wss[k] <- sum(kmeans(SAT_VM, centers = k, nstart = 25)$withinss)
wss
plot(1:11, wss, type = "b", pch = 16, col = "blue", xlab = "Number of clusters", ylab = "wss")

#### Chose 3 as the K value. Now plot the clusters ####
(SAT_VM.3means <- kmeans(SAT_VM, centers = 3, nstart = 10))
KM$withinss

##### 3B. Make a plot of the three clusters #####
plot(SAT_VM[SAT_VM.3means$cluster == 1, ], col = "red", 
     xlim=c(min(SAT_VM[ ,1]),max(SAT_VM[ ,1])),
     ylim=c(min(SAT_VM[ ,2]),max(SAT_VM[ ,2])))
points(SAT_VM[SAT_VM.3means$cluster == 2,], col = "blue")
points(SAT_VM[SAT_VM.3means$cluster == 3,], col = "seagreen")
points(SAT_VM.3means$centers,pch=2, col = "black")

##### 3C Change the scaling on the plot so that it goes from the full span of SAT scores: 200 - 800 ####
plot(SAT_VM[SAT_VM.3means$cluster == 1, ], col = "red", 
     xlim = c( 200, 800),
     ylim = c( 200, 800))
points(SAT_VM[SAT_VM.3means$cluster == 2,], col = "blue")
points(SAT_VM[SAT_VM.3means$cluster == 3,], col = "seagreen")
points(SAT_VM.3means$centers,pch=2, col = "black")


##### 4.Separate the SAT scores into 4 clusters Then Show the output from clusters. Make a plot of the 4 clusters. ####

##### Make 4 clusters####
(SAT_VM.4means <- kmeans(SAT_VM, centers = 4, nstart = 10))
SAT_VM.4means$withinss

##### Plot the 4 Clusters #####
plot(SAT_VM[SAT_VM.4means$cluster == 1, ], col = "red", 
     xlim = c( 200, 800),
     ylim = c( 200, 800))
points(SAT_VM[SAT_VM.4means$cluster == 2,], col = "blue")
points(SAT_VM[SAT_VM.4means$cluster == 3,], col = "seagreen")
points(SAT_VM[SAT_VM.4means$cluster == 4,], col = "purple")
points(SAT_VM.4means$centers,pch=2, col = "black")







