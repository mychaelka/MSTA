# Test
x1 <- c(3, 5, 6, 7, 9)
x2 <- c(7, 6, 8, 10, 9)

df <- data.frame(x1, x2)

euclid_dist <- dist(df, method = "euclidean")
cluster <- hclust(euclid_dist, method = "ward")
plot(cluster)

coph <- cophenetic(cluster)


##### PROJECT
library(DescTools) # to visualize pairwise correlation matrix
library(ellipse) # to visualize pairwise correlations matrix
library(car)
library(psych)

rm(list=ls())
load("Decathlon.RData")

data <- na.omit(Decathlon)[,15:24]
(R <- cor(data,use="pairwise.complete.obs"))
#pairs(Satisfaction,panel=panel.smooth)
scatterplotMatrix(data,smooth=F,diagonal="histogram",col=c(2,1,4))
PlotCorr(R)
plotcorr(R)

# is this an appropriate method? 
# is correlation matrix siginificantly different from unit matrix?
cortest.bartlett(R, n = 100, diag = TRUE)

### principal components
p<-prcomp(x=data, center=T,scale.=T)
a<-p$sdev^2
a

# percentage of total variance expl. by the first two components
sum(a[1:2])/10 
summary(p) 

### factors
f1<-fa(r=R,nfactors=3,rotate="varimax",fm="ml",scores="regression",residuals=T)
f2<-fa(r=R,nfactors=4,rotate="varimax",fm="ml",scores="regression",residuals=T)
f1
f2

plot(f1)
plot(f2)


f1$residual 
f1$weights
factor.scores(x=data,f1)$scores


fa_varimax <- factanal(data, factors = 3, rotation = "varimax")
plot(fa_varimax$loadings[,1], 
     fa_varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(fa_varimax$loadings[,1]-0.08, 
     fa_varimax$loadings[,2]+0.08,
     colnames(data),
     col="blue")
abline(h = 0, v = 0)

plot(fa_varimax$loadings[,1], 
     fa_varimax$loadings[,3],
     xlab = "Factor 1", 
     ylab = "Factor 3", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(fa_varimax$loadings[,1]-0.08, 
     fa_varimax$loadings[,3]+0.08,
     colnames(data),
     col="blue")
abline(h = 0, v = 0)

plot(fa_varimax$loadings[,2], 
     fa_varimax$loadings[,3],
     xlab = "Factor 2", 
     ylab = "Factor 3", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(fa_varimax$loadings[,2]-0.08, 
     fa_varimax$loadings[,3]+0.08,
     colnames(data),
     col="blue")
abline(h = 0, v = 0)
