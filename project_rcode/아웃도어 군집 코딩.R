setwd("C:/rdata")
customer_f <- read.csv("customer_fashion.csv")
rownames(customer_f) <- customer_f[,1]
customer_f <- customer_f[,-1]

customer_f.selected <- customer_f[, c("m", "x_idx")]
customer_f.selected.norm <- sapply(customer_f.selected[, c("m", "x_idx")], scale)

library(factoextra)
library(NbClust)
fviz_nbclust(customer_f.selected.norm[, c("m", "x_idx")], kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) +labs(subtitle = "Elbow method")

km <- kmeans(customer_f.selected.norm, 4)
km$cluster
km$size

centers <- aggregate(. ~ km$cluster, data = customer_f.selected[, c("m", "x_idx")], FUN = mean)
centers

cbind(centers, km$size)

customer_f["cluster_id"] <- km$cluster
write.csv(customer_f, "c:/rdata/customer_df.cluster.csv")
