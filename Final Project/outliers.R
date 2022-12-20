data <- read.delim("MVA-dataset.csv", sep=",",row.names = 1, header = TRUE)
sapply(data,"class")
summary(data)
####################################################################
# Missing values
####################################################################

##we decided to dropping observations with more than 60% missing values, which are: “Iraq”, “North Korea”, “Liechtenstein”, “Syria”, “Somalia”
dataset <- data[-79,][-88,][-98,][-151,][-158,]

dataset[!complete.cases(dataset),]

#Summary
summary(dataset)

# Column names
names(dataset)
###################### knn
library(DMwR)
knnOutput <- knnImputation(dataset)
result.knn = knnOutput[!complete.cases(dataset),]

#Exporting datasets of Imputation Missing values from R to compare them and choosing the best method in order to dealing with Missing Values
#write.csv(result.mice, "Missing-Values/result-mice.csv")
#write.csv(result.rf, "Missing-Values/result-rf.csv")
write.csv(result.knn, "Missing-Values/result-knn.csv")
write.csv(dataset, "Missing-Values/dataset.csv")


# dataset dimensions 
dim(dataset)

# Check factors
dataset.is.numeric <- unlist(lapply(names(dataset), function(col) is.numeric(dataset[,col])))
names(dataset.is.numeric) <- names(dataset)
dataset.is.numeric

#we just have 2 Categurical variables and the rest of them are numerical

library(chemometrics)
library(ggplot2)
library(dplyr)
myoutlier <- Moutlier(knnOutput[,1:27], quantile = 0.975, plot = TRUE)
myoutlier
text(myoutlier$rd, labels = rownames(data))

#venesuela, lybia

#MD <- mahalanobis(completeData, colMeans(completeData), cov(completeData))
#completeData$MD <- round(MD, 2)
#max(MD)
#mean(MD)

#summary(MD)
#completeData$outlierStatus <- FALSE
#completeData$outlierStatus[completeData$MD>155] <- TRUE
#completeData$Countries <- data$X
#venesuela, lybia

###########Outliers with Cooks distance
cooksdata <- knnOutput[1:27]
mod1 <- lm(Score ~ ., data=cooksdata)
cooksd1 <- cooks.distance(mod1)
plot(cooksd1, pch=".", cex=2, main="score")  # plot cook's distance
abline(h = 4*mean(cooksd1, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4*mean(cooksd1, na.rm=T),row.names(data),""), col="red")  # add labels

#venesuela, lybia,


