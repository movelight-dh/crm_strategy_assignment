setwd("c:/rdata")
customer.df <- read.csv("customer_df.cluster.age.churn.csv")

####create dummy variable
customer.df$sms_yn <- factor(customer.df$sms_yn, levels = c(0,1), labels = c("sms_N", "sms_Y"))

customer.df$email_yn <- factor(customer.df$email_yn, levels = c(0,1), labels = c("email_N", "email_Y"))

customer.df$dm_yn <- factor(customer.df$dm_yn, levels = c(0,1), labels = c("dm_N", "dm_Y"))

customer.df$wedding_yn <- factor(customer.df$wedding_yn, levels = c(0,1), labels = c("wedding_N", "wedding_Y"))


####subset cluster
customer.cluster1 <- subset(customer.df, cluster_id == 1)
customer.cluster2 <- subset(customer.df, cluster_id == 2)
customer.cluster3 <- subset(customer.df, cluster_id == 3)
customer.cluster4 <- subset(customer.df, cluster_id == 4)
customer.cluster3 <- rbind(customer.cluster3, customer.cluster4)

options(scipen = 999)
library(caret)
library(e1071)

#cluster_1 churn model
set.seed(1)
train.index1 <- sample(row.names(customer.cluster1), 0.6*dim(customer.cluster1)[1])
valid.index1 <- setdiff(row.names(customer.cluster1), train.index1)
train1.df <- customer.cluster1[train.index1, ]
train1.df
valid1.df <- customer.cluster1[valid.index1, ]
valid1.df

names(train1.df)
logit.cluster1.churn <- glm(churn ~ ., data=train1.df[,c("churn", "point", "r", "f", "m", "longevity", "email_yn", "dm_yn")])
summary(logit.cluster1.churn)

logit.cluster1.churn.pred <- predict(logit.cluster1.churn, valid1.df[,c("churn", "point", "r", "f", "m", "longevity", "email_yn", "dm_yn")], type ="response")

glm.pred1 <- rep(0, nrow(valid1.df))
glm.pred1[logit.cluster1.churn.pred>0.9] = 1

confusionMatrix(as.factor(valid1.df$churn), as.factor(glm.pred1), positive = '1')

#cluster_2 churn model
set.seed(1)
train.index2 <- sample(row.names(customer.cluster2), 0.6*dim(customer.cluster2)[1])
valid.index2 <- setdiff(row.names(customer.cluster2), train.index2)
train2.df <- customer.cluster2[train.index2, ]
train2.df
valid2.df <- customer.cluster2[valid.index2, ]
valid2.df

logit.cluster2.churn <- glm(churn ~ ., data=train2.df[,c("churn", "point", "r", "f", "m", "longevity", "email_yn", "dm_yn")])
summary(logit.cluster2.churn)

logit.cluster2.churn.pred <- predict(logit.cluster2.churn, valid2.df[,c("churn", "point", "r", "f", "m", "longevity", "email_yn", "dm_yn")], type ="response")
logit.cluster2.churn.pred

glm.pred2 <- rep(0, nrow(valid2.df))
glm.pred2[logit.cluster2.churn.pred>0.8] = 1

confusionMatrix(as.factor(valid2.df$churn), as.factor(glm.pred2), positive = '1')

#cluster_3 churn model
set.seed(1)
train.index3 <- sample(row.names(customer.cluster3), 0.6*dim(customer.cluster3)[1])
valid.index3 <- setdiff(row.names(customer.cluster3), train.index3)
train3.df <- customer.cluster3[train.index3, ]
train3.df
valid3.df <- customer.cluster3[valid.index3, ]
valid3.df

logit.cluster3.churn <- glm(churn ~ ., data=train3.df[,c("churn", "point", "r", "f", "m", "longevity", "email_yn", "dm_yn")])
summary(logit.cluster3.churn)

logit.cluster3.churn.pred <- predict(logit.cluster3.churn, valid3.df[,c("churn", "point", "r", "f", "m", "longevity", "email_yn", "dm_yn")], type ="response")
logit.cluster3.churn.pred

glm.pred3 <- rep(0, nrow(valid3.df))
glm.pred3[logit.cluster3.churn.pred>0.65] = 1

confusionMatrix(as.factor(valid3.df$churn), as.factor(glm.pred3), positive = '1')
