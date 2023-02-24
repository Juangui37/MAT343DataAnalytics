Project_4A <- read.csv("~/Library/CloudStorage/OneDrive-EasternConnecticutStateUniversity/3rd Year/Spring 2021/MAT 343 Data Analytics/Projects/Project4A/Project4A.R")
summary(titanic.raw)
str(titanic.raw)

summary(titanic.raw$Class)
summary(titanic.raw$Sex)
summary(titanic.raw$Age)
summary(titanic.raw$Survived)


(classtable <- table(titanic.raw$Class))
(agetable <- table(titanic.raw$Age))
(sextable <- table(titanic.raw$Sex))
(survivedtable <- table(titanic.raw$Survived))

#proportions (then percentages *100)
(classprop <- prop.table(classtable)*100)
(ageprop <- prop.table(agetable)*100)
(sexprop <- prop.table(sextable)*100)
(survivedprop <- prop.table(survivedtable)*100)
#Making sure no NA's
summary(titanic.raw)

#2 Start using R for association
install.packages('arules')
install.packages('aruleyessViz')
library(arules)
library(arulesViz)
library(igraph)
#I don't need the code below: Special for grocery data
#txn = read.transactions("titanic.raw",rm.duplicates = FALSE,format = "single",sep = ",",cols = c(1,2))

rules <- apriori(titanic.raw, parameter = list(minlen = 2, supp = .4,
                                               conf = .5, target = "rules"))
inspect(rules)
summary(rules)

rules_sorted <- sort(rules,by = "lift")
inspect(rules_sorted)

rules2 <- apriori(titanic.raw,parameter = list(minlen = 1, supp = .6,
                                               conf = .5, target = "rules"))
summary(rules2)


plot(rules2)


inspect(head(sort(rules, by = "lift",10)))

#####Question 2:# Survival Statues as the Consequent######


survival_rule <- apriori(titanic.raw,parameter = list(minlen = 2, support = .01,
                                                      conf = .8), appearance = list(default = "lhs", rhs = c("Survived=No", "Survived=Yes")))
inspect(survival_rule)                         
survival_rule_sorted <- sort(survival_rule, by = "lift")
inspect(survival_rule_sorted)

######Question 3: Finding Redundant Rules#########


subset_matrix <- is.subset(survival_rule_sorted, survival_rule_sorted)
subset_matrix[lower.tri(subset_matrix, diag=T)] <- NA
redundant <- colSums(subset_matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules_pruned <- survival_rule_sorted[!redundant]
inspect(rules_pruned)



###### Question 4: Make Several Graphs of your rules#######

plot(survival_rule_sorted, method = "graph", engine = "htmlwidget")  


plot(survival_rule_sorted)


plot(survival_rule_sorted, method = "two-key plot")

plot(survival_rule_sorted, method = "paracoord")

