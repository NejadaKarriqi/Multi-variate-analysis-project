mydataset <- read.delim("MVA-dataset.csv", header = TRUE, row.names = 1, sep=",")
summary(mydataset)

###########Outliers with Cooks distance
mod1 <- lm(Score ~ ., data=mydataset)
cooksd1 <- cooks.distance(mod1)
plot(cooksd1, pch=".", cex=2, main="score")  # plot cook's distance
abline(h = 4*mean(cooksd1, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4*mean(cooksd1, na.rm=T),names(cooksd1),""), col="red")  # add labels

######### check with mahalanobis distance ########
mydataset <- read.delim("MVA-dataset.csv", header = TRUE, row.names = 1, sep=",")
X <- mydataset[,1:27]

# define a function to find extreme outliers
FindExtremeOutliers <- function(data) {
  q1 = quantile(data)[2]
  q3 = quantile(data)[4]
  iqr = q3 - q1 #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 3) + q3
  extreme.threshold.lower = q1 - (iqr * 3)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}
#chech the missing values
mydataset[!complete.cases(mydataset),]

#imput missing values

imp = mice(X, method = "norm", m = 1, print = FALSE)
complete(imp)[!complete.cases(X),]

# mahalanobis

O = Moutlier(X, quantile = 0.95)
S = (O$md >= O$cutoff); cbind(dataset[S,], distance = O$md[S])





library(chemometrics)

outlier = Moutlier(mydataset[,1:27], quantile = 0.975, plot = TRUE)

outlier
to_cut <- outlier$rd[outlier$rd>75]
t <- row.names(as.matrix(to_cut))
Boobies_without <- Boobs[-which(rownames(Boobs) %in% t),]
par(mfrow=c(1,1))
boxplot(Boobies_without)
















