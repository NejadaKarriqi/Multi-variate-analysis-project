setwd("C:/Users/Quentin/Documents/FIB/Multvariate Analysis/Project")
boobs_raw <- read.table("Breast dataset.txt", header=FALSE, sep = ",", dec = ".")
head(boobs_raw)

set.seed(123456789)

colnames(boobs_raw) <- c("ID", "Diagnosis", "Mean radius", "Mean texture", "Mean perimeter", "Mean area", "Mean smoothness", "Mean compactness", "Mean concavity", "Mean concave points", "Mean symmetry", "Mean fractal dimension", "Radius SE", "Texture SE", "Perimeter SE", "Area SE", "Smoothness SE", "Compactness SE", "Concavity SE", "Concave points SE", "Symmetry SE", "Fractal dimension SE", "Largest radius", "Largest texture", "Largest perimeter", "Largest area", "Largest smoothness", "Largest compactness", "Largest concavity", "Largest concave points", "Largest symmetry", "Largest fractal dimension")
head(boobs_raw)
summary(boobs_raw)

Boobs <- data.frame(row.names = boobs_raw[,1], boobs_raw[2:32])
head(Boobs)
sapply(Boobs, class)

####################################################################
# FINDING OUTLIERS
####################################################################

boxplot(Boobs)

######MAHALANOBIS METHOD######

library(chemometrics)

outlier = Moutlier(Boobs[,2:31], quantile = 0.975, plot = TRUE)

outlier
to_cut <- outlier$rd[outlier$rd>75]
t <- row.names(as.matrix(to_cut))
Boobies_without <- Boobs[-which(rownames(Boobs) %in% t),]
par(mfrow=c(1,1))
boxplot(Boobies_without)

####################################################################
# SAMPLING TRAINING SET AND TEST SET
####################################################################

N <- nrow(Boobies_without)

learn <- sample(1:N, round(0.67*N))

nlearn <- length(learn)
ntest <- N - nlearn

####################################################################
# PRINCIPAL COMPONENT ANALYSIS
####################################################################


library(FactoMineR)
pca.boobs <- PCA(X = Boobies_without, quali.sup = c(1))

pca.boobs$eig
plot(pca.boobs$eig$eigenvalue, type= "l", main="Screeplot")

######MOST INFLUENCING VARIABLES######
#First dimension
sort(pca.boobs$var$contrib[,1], decreasing = TRUE)
#Second dimension
sort(pca.boobs$var$contrib[,2], decreasing = TRUE)

nd=4

####################################################################
# INTERPRETATION OF LATENT CONCEPTS
####################################################################

dimdesc(pca.boobs)

####################################################################
# CLUSTERING
####################################################################
Psi <- pca.boobs$ind$coord

d <- dist(Psi, method = "euclidean")
hc <- hclust(d, method = "ward.D2")
plot(hc)
barplot((hc$height))[(547-16):(562-1)]

nc = 5
c1 <- cutree(hc,nc)

#Consolidation operation
cdg <- aggregate(as.data.frame(Psi),list(c1),mean)
Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))
Tss <- sum(rowSums(Psi^2))
Tss/46
sum(pca.boobs$eig$eigenvalue[1:nd])
Ib4 <- 100*Bss/Tss
Ib4
# LETS CONSOLIDATE THE PARTITION
k5 <- kmeans(Psi,centers=cdg[2:6])
Bss <- sum(rowSums(k5$centers^2)*k5$size) # = k5$betweenss
Wss <- sum(k5$withinss) # = k5$tot.withinss
Ib5 <- 100*Bss/(Bss+Wss)
Ib5

####################################################################
# Q6 USING THE CATDES FUNCTION, INTERPRET THE CLUSTERS
####################################################################

catdes(cbind(as.factor(k5$cluster), Boobies_without),1)

plot(Psi[,1],Psi[,2],type="n",main="Clustering with consolidation operation")
text(Psi[,1],Psi[,2],col=k5$cluster, labels=rownames(Boobies_without),cex = 0.6)
abline(h=0,v=0,col="gray")
legend("bottomleft",c("c1","c2","c3","c4","c5"),pch=20,col=c(1:4))

