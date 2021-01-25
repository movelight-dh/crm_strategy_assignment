setwd("c:/rdata")
customer_df <- read.csv("customer_df.cluster.age.csv")
customer_df$churn_index <- as.numeric(as.character(customer_df$churn_index))

quantile(customer_df$churn_index, probs= seq(0,1,0.25), na.rm = TRUE)

churn_index <- data.frame(customer_df$X, customer_df$churn_index)
churn_index <- transform(churn_index,
                       churn_index = ifelse(customer_df$churn_index>1.3315, 1, 0))


customer_df["churn"] <- churn_index$churn_index                       

write.csv(customer_df, "c:/rdata/customer_df.cluster.age.churn.csv")