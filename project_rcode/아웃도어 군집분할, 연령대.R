setwd("c:/rdata")
customer_df.cluster<-read.csv("customer_df.cluster.csv")

customer.cluster1 <- subset(customer_df.cluster, cluster_id == 1)
customer.cluster2 <- subset(customer_df.cluster, cluster_id == 2)
customer.cluster3 <- subset(customer_df.cluster, cluster_id == 3)
customer.cluster4 <- subset(customer_df.cluster, cluster_id == 4)
min(customer_df.cluster$age)

age_group <- data.frame(customer_df.cluster$X, customer_df.cluster$age)
age_group <- transform(age_group,
                       age_group = cut(customer_df.cluster$age, breaks = c(10,20,30,40,50,60,70,80,90,100),
                                 include.lowest = TRUE,
                                 right = FALSE,
                                 labels = c("10대","20대","30대","40대", "50대", "60대", "70대", "80대", "90대")
                                 ))

customer_df.cluster["age_group"] <- age_group$age_group
names(customer_df.cluster)

write.csv(customer_df.cluster, "c:/rdata/customer_df.cluster.age.csv")