library(readxl)
data = read_csv("data.xlsx")
View(data)

library(tidyverse)
df = data %>% column_to_rownames(., var = 'Provinsi')
df = df[-nrow(df), ]
df

describe(df, quant=c(.25,.5,.75))
?describe

library(psych)
KMO(df)
r = cor(df)
cortest.bartlett(cor(df), n = nrow(df))

# Perform PCA
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

data_scaled = scale(df)
head(data_scaled)

corr_matrix = cor(data_scaled)

data.pca = prcomp(data_scaled, scale = F)
summary(data.pca)

fviz_eig(data.pca, addlabels = TRUE)
data.pca$rotation[,1:3]

data.pca = data.pca$x[,1:3]
head(data.pca)

# Perform Fuzzy C Means
library(ppclust)
library(dplyr)
library(cluster)
library(fclust)

fviz_nbclust(data.pca, kmeans, method=c('wss'))
fviz_nbclust(data.pca, kmeans, method=c('silhouette'))
fviz_nbclust(data.pca, kmeans, method=c('gap_stat'))


for (k in 2:7) {
  res.fcm = fcm(data.pca, centers=k)
  cat('\n',k,' Cluster')
  cat("FSI values",SIL.F(res.fcm$x,res.fcm$u))
  cat("\nPE Values",PE(res.fcm$u))
  cat("\nPC Values",PC(res.fcm$u))
  cat("\nMPC Values",MPC(res.fcm$u))
}

res.fcm <- fcm(data.pca, centers=2, nstart=5)

res.fcm$func.val

res.fcm$iter
res.fcm$best.start

last.fcm = fcm(data.pca, centers=2, nstart=5)
summary(last.fcm)

res.fcm2 <- ppclust2(last.fcm, "kmeans")
fviz_cluster(res.fcm2, data = df, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             )

df$Cluster = last.fcm$cluster
df

unique(data$Provinsi)
