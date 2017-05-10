
setwd("C:/Grad Case Studies/Leads Data")

set.seed(2017)

merged1 = read.csv("Columbusmerged_Leads.csv")
merged2=read.csv("Daytonmerged_Leads.csv")
merged3=read.csv("Cincinnatimerged_Leads.csv")
merged4=read.csv("Kentuckymerged_Leads.csv")
merged=rbind(merged1,merged2,merged3,merged4)
status = merged$status
status = as.character(status)
merged$response = NA



for (i in 1:length(merged[,1])){
  
   if (status[i]=="Sold"){
     merged$response[i] = 1
   }
  
  else {
    merged$response[i] = 0
  }
}

modata = merged[,c(15,16,38,39,40,41,42)]
modata$response = as.factor(modata$response)
modata$estimatetype=as.factor(modata$estimatetype)
modata$sourcetext=as.factor(modata$sourcetext)
modata$Population=as.numeric(as.character(modata$Population))
modata$Houses=as.numeric(as.character(modata$Houses))
modata$Income=as.numeric(as.character(modata$Income))
modata$Per.Capita.Income=as.numeric(as.character(modata$Per.Capita.Income))




subset = sample(nrow(modata), nrow(modata) * 0.75)
train = modata[subset, ]
test = modata[-subset, ]

del = is.na(train$Population)
train = train[!del,]
del = is.na(test$Population)
test = test[!del,]

library(rpart)
credit.rpart1 <- rpart(formula = response ~ estimatetype+sourcetext+Income, data = train, method = "class")

plot(credit.rpart1)
text(credit.rpart1)

credit.rpart <- prune(credit.rpart1, cp = 0.15)
plot(credit.rpart)
text(credit.rpart, cex=0.7) # text and plot need to be one after one another

credit.test.pred.tree1 = predict(credit.rpart, train, type = "class")

table(train$response, credit.test.pred.tree1, dnn = c("Truth", "Predicted"))

mean(ifelse(train$response != credit.test.pred.tree1, 1, 0))

credit.test.pred.tree2 = predict(credit.rpart, test, type = "class")

table(test$response, credit.test.pred.tree2, dnn = c("Truth", "Predicted"))

mean(ifelse(test$response != credit.test.pred.tree2, 1, 0))


split.fun<-function(x,labs,digits,varlen,faclen)
{
  labs = gsub(",","",labs)
  for (i in length(labs)){
    labs[i]=paste(strwrap(labs[i],width=25),collapse="\n")
  }
  labs
  
  
}

library(rpart.plot)

prp(credit.rpart1,split.fun=split.fun)

library(tree)  
library(ROCR)
credit.test.prob.tree1 = predict(credit.rpart, test, type = "prob")
pred_test_rpart <- prediction(credit.test.prob.tree1[, 2], test$response)
perf_test_rpart <- performance(pred_test_rpart, "tpr", "fpr")
plot(perf_test_rpart, colorize = TRUE, main = "ROC Curve: Testing Data")
as.numeric(performance(pred_test_rpart, 'auc')@y.values)

credit.test.prob.tree1 = predict(credit.rpart, train, type = "prob")
pred_test_rpart <- prediction(credit.test.prob.tree1[, 2], train$response)
perf_test_rpart <- performance(pred_test_rpart, "tpr", "fpr")
plot(perf_test_rpart, colorize = TRUE, main = "ROC Curve: Training Data")
as.numeric(performance(pred_test_rpart, 'auc')@y.values)

# logistic

german_credit_train.glm_Full <- glm(response ~ ., family = binomial(link = "logit"), train)
german_credit_train.glm_Null <- glm(response ~ 1, family = binomial(link = "logit"), train)
# stepwise Selection Method
german.credit.train.step.aic = step(german_credit_train.glm_Null, 
                                    scope = list(lower = german_credit_train.glm_Null,upper=german_credit_train.glm_Full),direction="both")

german_credit.glm.prob.insample <- predict(german.credit.train.step.aic, type = "response")
german_credit.glm.pred.insample  <- (german_credit.glm.prob.insample > (1/2) ) * 1
table(train$response, german_credit.glm.pred.insample, dnn = c("Truth", "Predicted"))
mean(ifelse(train$response != german_credit.glm.pred.insample, 1, 0))

library(ROCR)
pred_train <- prediction(german_credit.glm.pred.insample, train$response)
perf_Train <- performance(pred_train, "tpr", "fpr")
plot(perf_Train, colorize = TRUE)
as.numeric(performance(pred_train, 'auc')@y.values)

# Out of Sample
german_credit.glm.prob.outsample <- predict(german.credit.train.step.aic, test, type = "response")
german_credit.glm.pred.outsample  <- (german_credit.glm.prob.outsample > (1/2) ) * 1
table(test$response, german_credit.glm.pred.outsample, dnn = c("Truth", "Predicted"))


#mis-classification rate
mean(ifelse(test$response != german_credit.glm.pred.outsample, 1, 0))


pred_test <- prediction(german_credit.glm.pred.outsample, test$response)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, colorize = TRUE)
as.numeric(performance(pred_test, 'auc')@y.values)


library(randomForest)
output.forest <- randomForest(response~., 
                              data = train)

credit.test.pred.treeran = predict(output.forest, train, type = "class")

table(train$response, credit.test.pred.treeran, dnn = c("Truth", "Predicted"))

mean(ifelse(train$response != credit.test.pred.treeran, 1, 0))

credit.test.pred.treeran1 = predict(output.forest, test, type = "class")

table(test$response, credit.test.pred.treeran1, dnn = c("Truth", "Predicted"))

mean(ifelse(test$response != credit.test.pred.treeran1, 1, 0))
