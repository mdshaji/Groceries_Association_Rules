# Installed Required packages for Association Rules

library(arules) # Library used for Association rules Algorithm
library(arulesViz) # Used for visualization on Ass.Rules Algorithm
library(treemap) # space-filling visualization of hierarchical structures
library(dplyr) # data manipulation

# Importing the dataset
Groceries <- read.transactions("D:/Semester-2-CourseWork/Assignment-DataSets/Task2/groceries.csv", format = "basket", sep = ",", skip=1)
Groceries1 <- read.csv("D:/Semester-2-CourseWork/Assignment-DataSets/Task2/groceries.csv")

#returns the loaded dataset
data("Groceries")

# Inspecting the data for further analysis
cat("No of baskets:", length(Groceries)) # concatenate string & integer value
cat("No of unique items:", sum(size(Groceries)))
head(itemInfo(Groceries)) # Top 5 records


#plot for most frequent items in transactions
itemFrequencyPlot(Groceries, topN=10, type="relative", main="Frequency of items", col=rainbow(7), cex.names=0.8)

# plot of 10 most least frequent items bought by Cux
head(sort(itemFrequency(Groceries), decreasing=FALSE), n=10)

# histogram for no of items in each basket
hist(size(Groceries), breaks = 0:35, xaxt="n", ylim=c(0,2500), 
     main = "No of Items in Each basket",col = rainbow(10), xlab = "items")
axis(1, at=seq(0,35,by=5), cex.axis=0.8)

# ncol returns the no of item columns in each basket
cat("The biggest basket consists of", ncol(Groceries1), "items")

# Using Eclat Algorithm

freq.itemsets <- eclat(Groceries, parameter=list(supp=0.01, maxlen=15))
inspect(head(freq.itemsets))

# Creating rules for above itemsets
rules <- ruleInduction(freq.itemsets, Groceries, confidence=1)
rules

rules <- ruleInduction(freq.itemsets, Groceries, confidence=0.5)
rules

inspect(rules)
is.significant(rules, Groceries)

arulesViz::plotly_arules(rules, method="matrix", measure=c("support","confidence")) # Matrix plot for above 15 rules
plot(rules, method="grouped") # Group Matrix plot
plot(rules, method="graph", col=rainbow(7), shading="lift") # Visualizing rules in a form of graph
plot(rules, measure=c("support", "confidence"), shading="lift", interactive=FALSE) # Scatterplot

# Using Apriori Algorithm

rules1 <- apriori(Groceries, parameter=list(supp=0.01, conf=0.5, maxlen=15))
inspect(rules1) 

is.significant(rules1, Groceries)

arulesViz::plotly_arules(rules1, method="matrix", measure=c("support","confidence"))

plot(rules1, method="grouped") # Group Matrix plot

plot(rules1, method="graph", shading="lift") 

plot(rules1, measure=c("support", "confidence"), shading="lift", interactive=FALSE) # Scatterplot

# Forming the rules for individual items

# Rules for Wholemilk
milk_rules <- apriori(data=Groceries,  parameter=list(supp=0.01, conf = 0.5, target="rules"), appearance = list(default="lhs", rhs="whole milk"), control=list(verbose=F)) 
milk_rules_byconf <- sort(milk_rules, by="confidence", decreasing=TRUE)
inspect(milk_rules_byconf)

plot(milk_rules, method="graph", cex=0.7, shading="lift",control=list(col=rainbow(7)))# graph based on confidence
plot(milk_rules , method="paracoord")# parallel coordinates plot based on support
                                                                    
# Rules for Yogurt
yogurt_rules <- apriori(data=Groceries,  parameter=list(supp=0.01, conf = 0.3, target="rules"), appearance = list(default="lhs", rhs="yogurt"), control=list(verbose=F)) 
yogurt_rules_byconf <- sort(yogurt_rules, by="confidence", decreasing=TRUE)
inspect(head(yogurt_rules_byconf))

plot(yogurt_rules, method="graph", cex=0.7, shading="lift") # graph based on support and lift
plot(yogurt_rules, method="paracoord") # parallel coordinates plot based on support

# Hierarchical rules

unique(Groceries@itemInfo[["level1"]]) # finding the unique items of level 1 in dataset
unique(Groceries@itemInfo[["level2"]]) # finding the unique items of level 2 in dataset

# Forming treemap visualization for Level 1
treemap1 <- Groceries@itemInfo %>% group_by(level1) %>% summarize(n=n()) 
treemap(treemap1,index=c("level1"),vSize="n",title="",palette="Dark2",border.col="#FFFFFF")

# Forming treemap visualization for Level 2
treemap2 <- Groceries@itemInfo %>% group_by(level1, level2) %>% summarize(n=n())
treemap(treemap2,index=c("level1", "level2"),vSize="n",title="",palette="Dark2",border.col="#FFFFFF")

agg_level2 <- aggregate(Groceries, by="level2") # finding aggregate of level 2 as there are more levels in it.
inspect(head(agg_level2))

agg_level2_rules <- apriori(data = agg_level2,  parameter=list(supp=0.05, conf = 0.5, target="rules"), control=list(verbose=F)) 
agg_level2_rules_byconf <- sort(agg_level2_rules, by="confidence", decreasing=TRUE)
inspect(head(agg_level2_rules_byconf))

plot(agg_level2_rules, method="graph", cex=0.7, shading="lift") # graph based on support and lift


multilevel <- addAggregate(Groceries, "level2")
inspect(head(multilevel)) # the * indicates group-level items
multilevel_rules <- apriori(data = multilevel,  parameter=list(supp=0.05, conf = 0.5, target="rules"), control=list(verbose=F))
inspect(head(multilevel_rules))

rules <- filterAggregate(multilevel) # using filteraggregate function to filter any transactions that are incorrect in nature
rules

# Calculating dissimilarity using 'Jaccard' function

transaction <- Groceries[,itemFrequency(Groceries)>0.1]
jaccard <- dissimilarity(transaction, which="items", method = "jaccard")
round(jaccard, 2)

# Calculating similarity using 'affinity' function
affinit <- affinity(transaction)
round(affinit, 2) 

# Graphical representation for above findings
image(affinit, axes = FALSE)
axis(1, at=seq(0,1,l=ncol(affinit)), labels=rownames(affinit), cex.axis=0.6, las=2)
axis(2, at=seq(0,1,l=ncol(affinit)), labels=rownames(affinit), cex.axis=0.6, las=1.5)


