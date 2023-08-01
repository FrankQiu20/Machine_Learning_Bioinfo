#####Load package
library(RSBID)
library(randomForest)
library(datasets)
library(caret)
library(pROC)
library(ROCR)
library(e1071)
library(rpart)
library(naivebayes)
#####Load data
data("abalone")
#####
ROS(abalone,outcome = 8)
data<-abalone


#### Randomforest
### original
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  model <- randomForest(Class~., data=train, proximity=TRUE)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### ROS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- ROS(train,8)
  model <- randomForest(Class~., data=train, proximity=TRUE)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### RUS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- RUS(train,8)
  model <- randomForest(Class~., data=train, proximity=TRUE)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SBC
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SBC(train,8)
  model <- randomForest(Class~., data=train, proximity=TRUE)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SMOTE
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SMOTE(train,8)
  model <- randomForest(Class~., data=train, proximity=TRUE)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

####SVM

### original
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  model <- svm(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### ROS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- ROS(train,8)
  model <- svm(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}
mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### RUS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- RUS(train,8)
  model <- svm(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}
mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SBC
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SBC(train,8)
  model <- svm(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}
mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SMOTE
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SMOTE(train,8)
  model <- svm(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

####decision tree
### original
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  model <- rpart(Class~., data=train, method = 'class')
  p1 <- predict(model, test,type='class')
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### ROS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- ROS(train,8)
  model <- rpart(Class~., data=train, method = 'class')
  p1 <- predict(model, test,type='class')
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### RUS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- RUS(train,8)
  model <- rpart(Class~., data=train, method = 'class')
  p1 <- predict(model, test,type='class')
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SBC
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SBC(train,8)
  model <- rpart(Class~., data=train, method = 'class')
  p1 <- predict(model, test,type='class')
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SMOTE
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SMOTE(train,8)
  model <- rpart(Class~., data=train, method = 'class')
  p1 <- predict(model, test,type='class')
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

###Naive bayes

### original
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  model <- naiveBayes(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### ROS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- ROS(train,8)
  model <- naiveBayes(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### RUS
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- RUS(train,8)
  model <- naiveBayes(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SBC
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:100) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SBC(train,8)
  model <- naiveBayes(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

### SMOTE
Accuracy<-rep(0,100)
Sensitivity<-rep(0,100)
Specificity<-rep(0,100)
AUC<-rep(0,100)

for (i in 1:2) {
  set.seed(i)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  train <- SMOTE(train,8)
  model <- naiveBayes(Class~., data=train)
  p1 <- predict(model, test)
  CM<-confusionMatrix(p1, test$Class)
  Accuracy[i]<-CM$overall[1]
  Sensitivity[i]<-CM$byClass[1]
  Specificity[i]<-CM$byClass[2]
  AUC[i]<-auc(test$Class,as.numeric(p1))
}

mean(Accuracy)
mean(Sensitivity)
mean(Specificity)
mean(AUC)

dat<-data.frame(Metric=c(rep("Accuracy",5),
                         rep("Sensitivity",5),
                         rep("Specificity",5),
                         rep("AUC",5)),
                Method = c(rep(c("Original","SBC","ROS","RUS","SMOTE"),4)),
                Value = c(0.95,0.688,0.945,0.775,0.931,
                          0.083,1,0.167,0.667,0.25,
                          1,0.67,0.99,0.782,0.971,
                          0.769,0.923,0.772,0.721,0.699))

library(ggplot2)
ggplot(dat)+aes(x=Metric,y=Value,fill=Method)+
  geom_bar(stat='identity',position = 'dodge')+labs(title="Random forest")+
  theme_classic()

dat<-data.frame(Metric=c(rep("Accuracy",5),
                         rep("Sensitivity",5),
                         rep("Specificity",5),
                         rep("AUC",5)),
                Method = c(rep(c("Original","SBC","ROS","RUS","SMOTE"),4)),
                Value = c(0.945,0.697,0.743,0.803,0.963,
                          0,0.833,0.833,0.833,0.333,
                          1,0.689,0.738,0.801,1,
                          0.502,0.789,0.806,0.826,0.665))

library(ggplot2)
ggplot(dat)+aes(x=Metric,y=Value,fill=Method)+
  geom_bar(stat='identity',position = 'dodge')+labs(title="SVM")+
  theme_classic()

dat<-data.frame(Metric=c(rep("Accuracy",5),
                         rep("Sensitivity",5),
                         rep("Specificity",5),
                         rep("AUC",5)),
                Method = c(rep(c("Original","SBC","ROS","RUS","SMOTE"),4)),
                Value = c(0.922,0.748,0.881,0.697,0.908,
                          0.083,0.667,0.417,0.833,0,
                          0.971,0.908,0.864,0.689,0.961,
                          0.623,0.769,0.735,0.721,0.696))

library(ggplot2)
ggplot(dat)+aes(x=Metric,y=Value,fill=Method)+
  geom_bar(stat='identity',position = 'dodge')+labs(title="Decision tree")+
  theme_classic()



dat<-data.frame(Metric=c(rep("Accuracy",5),
                         rep("Sensitivity",5),
                         rep("Specificity",5),
                         rep("AUC",5)),
                Method = c(rep(c("Original","SBC","ROS","RUS","SMOTE"),4)),
                Value = c(0.876,0.61,0.743,0.807,0.904,
                          0.417,0.833,0.833,0.583,0.25,
                          0.903,0.597,0.738,0.82,0.942,
                          0.66,0.779,0.801,0.760,0.597))

library(ggplot2)
ggplot(dat)+aes(x=Metric,y=Value,fill=Method)+
  geom_bar(stat='identity',position = 'dodge')+labs(title="Naive Bayes")+
  theme_classic()
