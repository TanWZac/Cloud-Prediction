setwd("C:/Users/Acer/Desktop/2021/FIT3152/Data")

rm(list = ls())
WAUS = read.csv("CloudPredict2021.csv", stringsAsFactors = T)
L = as.data.frame(c(1:49))
set.seed(30504457) 
L = L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS = WAUS[(WAUS$Location %in% L),]
WAUS = WAUS[sample(nrow(WAUS), 2000, replace = FALSE),]

library(tree)
library(ROCR)
library(e1071)
library(rpart)
library(adabag)
library(AUC)
library(randomForest)
library(varImp)
library(car)
library(plotrix)

attach(WAUS)

na_count = sapply(WAUS, function(y) sum(length(which(is.na(y)))))
na_count



Portion = WAUS[!is.na(CloudTomorrow), "CloudTomorrow"]
frq = table(Portion)/length(Portion)
-sum(frq * log2(frq))
frqs = table(WAUS$CloudTomorrow)/length(WAUS$CloudTomorrow)
-sum(frqs * log2(frqs))

# cor hist

SD = cbind(aggregate(WAUS[5:9], WAUS[4], sd, na.rm = T), aggregate(WAUS[14:21], WAUS[4], sd, na.rm = T))
SD[7] = NULL
SD
Mean = cbind(aggregate(WAUS[5:9], WAUS[4], mean, na.rm = T), aggregate(WAUS[14:21], WAUS[4], mean, na.rm = T))
Mean[7] = NULL
Mean


one = length(WAUS[((CloudTomorrow == 1)&(!is.na(CloudTomorrow))), "CloudTomorrow"])
zero = length(WAUS[((CloudTomorrow == 0)&(!is.na(CloudTomorrow))), "CloudTomorrow"])
out = cbind(one, zero)
colnames(out) = c(1, 0)
pie3D(out, labels = out , col = c("lightblue", "orange"))
legend("topleft", legend=c("Cloudy Tomorrow", "Not Cloudy Tomorrow"), pch =19 , col = c("lightblue", "orange"))
title("Proportion of cloudy days to clear days")



NAV = function(x){
  for (i in 1:14){
    x[i][is.na(x[i])] = rnorm(sum(is.na(x[i])), mean = mean(x[,i], na.rm = T), sd = sd(x[,i], na.rm = T))
  }
  return(x)
}
num = cbind(WAUS[5:9], WAUS[11])
num = cbind(num, WAUS[14:21])
com = NAV(num)

par(oma = c(10, 4, 4, 2))
heatmap(cor(NAV(num)))
library(Hmisc)
par(oma = c(0,0,0,0))
hist.data.frame(NAV(com))

com = cbind(com, WAUS[c(10, 12, 13, 22:23)])
com = cbind(WAUS[2], com)
df = com[complete.cases(com),]
df$CloudTomorrow = as.factor(df$CloudTomorrow)
df$RainToday = recode(df$RainToday, " 'Yes' = '1'; 'No' = '0'")


set.seed(30504457)
train.row = sample(1:nrow(df), 0.7*nrow(df))
data.train = df[train.row,]
data.test = df[-train.row,]



# Decision tree
set.seed(30504457)
train.tree = tree(CloudTomorrow~., data = data.train)
plot(train.tree)
text(train.tree, pretty = 8)
Predicted.Tree = predict(train.tree, data.test, type = "class")
Tree.CM = table(Actual = data.test$CloudTomorrow, Predicted = Predicted.Tree)
Tree.CM

# Naive bayes
set.seed(30504457)
train.naive.bayes = naiveBayes(CloudTomorrow~., data = data.train)
Predicted.naive.bayes = predict(train.naive.bayes, data.test)
naive.bayes.CM = table(Actual = data.test$CloudTomorrow, Predicted = Predicted.naive.bayes)
naive.bayes.CM

# Random Forest
set.seed(30504457)
train.random.forest = randomForest(CloudTomorrow~., data.train)
Predicted.random.forest = predict(train.random.forest, data.test)
random.forest.CM = table(Actual = data.test$CloudTomorrow, Predicted = Predicted.random.forest)
random.forest.CM

# Bagging
set.seed(30504457)
train.bagging = bagging(CloudTomorrow~., data = data.train, mfinal = 5)
Predicted.bagging = predict.bagging(train.bagging, data.test)
bagging.CM = table(Actual = data.test$CloudTomorrow, Predicted = Predicted.bagging$class)
bagging.CM

# Boosting
set.seed(30504457)
train.boosting = boosting(CloudTomorrow ~., data = data.train, mfinal = 10)
Predicted.boosting = predict.boosting(train.boosting, data.test)
boosting.CM = table(Actual = data.test$CloudTomorrow, Predicted = Predicted.boosting$class)
boosting.CM


set.seed(30504457)
Predict.Plot.Tree = predict(train.tree, data.test, type = 'vector')
outcome.tree = prediction(Predict.Plot.Tree[,2], data.test$CloudTomorrow)
Tree.perform = performance(outcome.tree, "tpr", "fpr")
plot(Tree.perform, col = "blue")

set.seed(30504457)
Predict.Plot.nb = predict(train.naive.bayes, data.test, type = 'raw')
outcome.nb = prediction(Predict.Plot.nb[,2], data.test$CloudTomorrow)
NB.perform = performance(outcome.nb, "tpr", "fpr")
plot(NB.perform, add = T, col = "blueviolet")

set.seed(30504457)
Predict.Plot.rf = predict(train.random.Forest, data.test, type = 'prob')
outcome.random.forest = prediction(Predict.Plot.rf[,2], data.test$CloudTomorrow) 
Random.forest.perform = performance(outcome.random.forest, "tpr", 'fpr')
plot(Random.forest.perform, add = T, col = "Darkgreen")

set.seed(30504457)
outcome.bagging = prediction(Predicted.bagging$prob[, 2], data.test$CloudTomorrow)
Bagging.perform = performance(outcome.bagging, "tpr", "fpr")
plot(Bagging.perform, add = T, col = "red")

set.seed(30504457)
outcome.boosting = prediction(Predicted.boost$prob[,2], data.test$CloudTomorrow)
Boosting.perform = performance(outcome.boosting, "tpr", "fpr")
plot(Boosting.perform, add = T, col = "orange")

title('ROC of classification models')
legend("bottomright", legend = c("Decision Tree","Naive Bayes", "Random Forest", "Bagging", "Boosting"), pch = 19,col=c("blue", "blueviolet", "Darkgreen", "red", "orange"))
abline(0,1)

# AUC
as.numeric(performance(outcome.tree, "auc")@y.values)       # decision tree
as.numeric(performance(outcome.nb, "auc")@y.values)         # Naive Bayes
auc(roc(outcome.random.forest[,2], data.test[complete.cases(data.test),]$CloudTomorrow))   # random forest
auc(roc(outcome.bagging[,2], data.test$CloudTomorrow)) # bagging
auc(roc(outcome.boosting[,2], data.test$CloudTomorrow))   # boosting



Accuracy = function(x){
  return(sum(diag(x))/sum(x))
}
Accuracy(Tree.CM)
Accuracy(naive.bayes.CM)
Accuracy(random.forest.CM)
Accuracy(bagging.CM)
Accuracy(boosting.CM)



print(summary(train.tree))
# WindGustDir WindDir3pm WindDir9am Humidity9am  WindSpeed9am  Pressure3pm WindGustSpeed MinTemp Pressure9am
Random_Forest_Variable_Importance = train.random.Forest
varImpPlot(Random_Forest_Variable_Importance, type = 2)
par(mar=c(11,4,4,4))
barplot(t(importance(Random_Forest_Variable_Importance))/sum(importance(Random_Forest_Variable_Importance)), las =2)
title("Random Forest Variable Importance")
# WindGistDir WindDir9am WindDir3pm Temp9am Sunshine MaxTemp Temp3pm
barplot(train.bagging$importance[order(-train.bagging$importance)], las = 2)
title("Bagging Variable Importance")
# WindGustDir WindDir3pm WindDir9am Humidity3pm Temp3pm Temp9am Evaporation Sunshine
barplot(train.boost$importance[order(-train.boost$importance)], las = 2) 
title("Boosting Variable Importance")
# WindGustDir WindDir3pm WindDir9am Sunshine WindGustSpeed Evaporation Humidity3pm 


set.seed(30504457)
# -Month-RainToday-Rainfall
Improve.tree = tree(CloudTomorrow~.-Month-RainToday-Rainfall, data = data.train)
prune.Tree = prune.misclass(Improve.tree, best = 4)
plot(prune.Tree)
text(prune.Tree, pretty = 0)
title("Pruned Decision Tree")
Predict.Tree.p = predict(prune.Tree, data.test, type = 'vector')
outcomes.tree = prediction(Predict.Tree.p[,2], data.test$CloudTomorrow)
as.numeric(performance(outcomes.tree, "auc")@y.values)       # decision tree
Prune.Tree.CM = predict(prune.Tree, data.test, type = 'class')
Pruned.Tree.CM = table(Actual = data.test$CloudTomorrow, Predicted = Prune.Tree.CM)
Accuracy(Pruned.Tree.CM)

set.seed(30504457)
train.tree = tree(CloudTomorrow~., data = data.train)
plot(train.tree)
text(train.tree, pretty = 0)
Predict.Plot.Tree = predict(train.tree, data.test, type = 'vector')
outcome.tree = prediction(Predict.Plot.Tree[,2], data.test$CloudTomorrow)
as.numeric(performance(outcome.tree, "auc")@y.values)
Accuracy(Tree.CM)

# ROC plot
Tree.perform = performance(outcomes.tree, "tpr", "fpr")
plot(Tree.perform, col = "red")
Tree.perform = performance(outcome.tree, "tpr", "fpr")
plot(Tree.perform, add = T,col = "blue")
title("ROC between prune and unpruned decision tree")
legend("bottomright", legend = c("Pruned Decision Tree","Unpruned Decision Tree"), pch = 19,col=c("red", "blue"))
abline(0,1)



set.seed(30504457)
df$Rainfall = NULL
df$RainToday = NULL
df$Month = NULL

train.row = sample(1:nrow(df), 0.7*nrow(df))
data.train = df[train.row,]
data.test = df[-train.row,]

Best = bagging.cv(CloudTomorrow~.-Sunshine-Evaporation, data=data.train, v= 20 , mfinal = 35)
Accuracy(Best$confusion)

# Don't run the for loop, it took me 4 hrs to produce the result
V = c(10, 15, 20, 25)
M = c(20, 25, 30, 35, 40, 45, 50) # 20 35
result = c()
Best.Tree = function(V, M, FUN){
  for (i in V){
    result = c(result, i)
    for (j in M){
      set.seed(30504457)
      tmp = bagging.cv(CloudTomorrow~.-Sunshine-Evaporation, data=data.train, v= i , mfinal = j)
      result = c(result, FUN(tmp$confusion))
    }
  }
  return(result)
}
Do = Best.Tree(V, M, FUN = Accuracy)
Do


library(neuralnet)
CT.nn = neuralnet(CloudTomorrow == 1 ~ MinTemp+MaxTemp+Evaporation+WindGustSpeed+Sunshine+WindSpeed9am+WindSpeed3pm+Humidity3pm+Humidity9am+Pressure3pm+Pressure9am+Temp9am+Temp3pm, data.train, hidden = 6, linear.output = F)
CT.pred = compute(CT.nn, data.test[c(1:13)])
CT.prob = CT.pred$net.result
prediction = ifelse(CT.prob>0.5, 1, 0)
NN = table(actual = data.test$CloudTomorrow, predicted = prediction)
Accuracy(NN)

ct.nn = neuralnet(CloudTomorrow == 1~Humidity9am+WindSpeed9am+Pressure3pm+WindGustSpeed+MinTemp+Pressure9am, data.train, hidden = 6, linear.output = F)
ct.pred = compute(ct.nn, data.test[c(8, 6, 5, 1, 10, 11)])
ct.prob = ct.pred$net.result
pd = ifelse(ct.prob>0.5, 1, 0)
nn = table(actual = data.test$CloudTomorrow, predicted = pd)
Accuracy(nn)

Sub = WAUS[5:23]
df = Sub[complete.cases(Sub),]
df$CloudTomorrow = as.factor(df$CloudTomorrow)
CTmm = model.matrix(~WindGustDir+WindDir3pm+WindDir9am, data = df)
CTcombine = cbind(df, CTmm)
CTcombine = CTcombine[, c(1,2,3,7,10:18,22:50,19)]
Cat.nn = CTcombine[,c(14:43)]
set.seed(30504457)
sample = sample(1:nrow(Cat.nn), 0.8*nrow(Cat.nn))
ANN.train = Cat.nn[sample,]
ANN.test = Cat.nn[-sample,]

cat.nn = neuralnet(CloudTomorrow~.,data=ANN.train, hidden= 2, linear.output = F)
cat.nn.pred = compute(cat.nn, ANN.test)
cat.nn.final = ifelse(cat.nn.pred$net.result > 0.5, 1, 0)

a = table(actual = ANN.test$CloudTomorrow, predicted = cat.nn.final[,1])
Accuracy(a)

b = table(actual = ANN.test$CloudTomorrow, predicted = cat.nn.final[,2])
Accuracy(b)

Sub = WAUS[5:23]
df = Sub[complete.cases(Sub),]
df$CloudTomorrow = as.factor(df$CloudTomorrow)
CTmm = model.matrix(~WindGustDir+WindDir3pm+WindDir9am, data = df)
df$RainToday = recode(df$RainToday, " 'Yes' = '1'; 'No' = '0'")
df$RainToday = as.numeric(df$RainToday)
CTcombine = cbind(df, CTmm)
CTcombine = CTcombine[, c(1,2,3,7,10:18,22:50,19)]
set.seed(30504457)
sample = sample(1:nrow(CTcombine), 0.8*nrow(CTcombine))
CT.train = CTcombine[sample,]
CT.test = CTcombine[-sample,]

CT.nn = neuralnet(CloudTomorrow~., CT.train, hidden = 4, linear.output = F)
CT.pred = compute(CT.nn, CT.test[,c(1:42)])
CT.prob = CT.pred$net.result
prediction = round(CT.prob, 0)
NN.try = table(actual = CT.test$CloudTomorrow, predicted = prediction[,1])
Accuracy(NN.try)
NN.try.2 = table(actual = CT.test$CloudTomorrow, predicted = prediction[,2])
Accuracy(NN.try.2)
