#Now I'm going to create a new data frame to manipulate some categorical variables into numeric
DummyMidtermData <- MidtermData
DummyMidtermData$Retained <- ifelse(DummyMidtermData$Retained == 'Y', 1, 0)
DummyMidtermData$Gender <- ifelse(DummyMidtermData$Gender == 'M', 1, 0)

summary(MidtermData)
summary(DummyMidtermData)


(corr <- cor(DummyMidtermData[,-2], use = "pairwise.complete.obs", method = "pearson"))

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = FALSE, title="Correlogram of Tuition Variables")
