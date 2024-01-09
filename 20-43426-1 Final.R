mydata<- read.csv('D:/Dataset.csv', header = TRUE , sep = ",")
mydata
options(max.print=9999)
names(mydata)

str(mydata)

summary(mydata)

colSums(is.na(mydata))

sum(duplicated(mydata))
mydata$SEX <- factor(mydata$SEX, levels = c("F","M"), labels = c(1,2))
mydata
mydata$SEX = as.numeric(mydata$SEX)
mydata$THROMBOCYTE = as.numeric(mydata$THROMBOCYTE)
mydata$AGE = as.numeric(mydata$AGE)
mydata$SOURCE = as.numeric(mydata$SOURCE)

scale(mydata)
my_norm <- mydata
for(i in 1:ncol(mydata))
norm_minmax <- function(x){(x- min(x)) /(max(x)-min(x))}
my_norm <- as.data.frame(lapply(my_norm, norm_minmax))
my_norm
print(my_norm)
head(my_norm)
(cor(my_norm$HAEMATOCRIT, my_norm$HAEMOGLOBINS))
(cor(my_norm$HAEMATOCRIT, my_norm$ERYTHROCYTE))
(cor(my_norm$ERYTHROCYTE, my_norm$HAEMOGLOBINS))
(cor(my_norm$HAEMATOCRIT, my_norm$LEUCOCYTE))
(cor(my_norm$LEUCOCYTE, my_norm$THROMBOCYTE))
(cor(my_norm$MCH, my_norm$THROMBOCYTE))
(cor(my_norm$MCV, my_norm$MCHC))
(cor(my_norm$MCV, my_norm$AGE))
(cor(my_norm$SEX, my_norm$AGE))
(cor(my_norm))

install.packages("corrplot")
library(corrplot)
correlation_matrix <- cor(my_norm)
corrplot(correlation_matrix, method = "circle")

correlation_matrix1 <- cor(my_norm)
less_correlated <- which(abs(correlation_matrix1) < 0.1)
attributes_to_remove <- c(less_correlated) 
normdata <- my_norm[, -attributes_to_remove]
normdata


accuracy <- numeric(10)
all_fold_predicted_data <- character(0)  
all_labels <- character(0)
for (fold in 1:10) {
 
  fold_data <- sample(seq_len(nrow(normdata)), size = floor(0.1 * nrow(normdata)))
  test_data <- normdata[fold_data, ]
  train_data <- normdata[-fold_data, ]
  
  library(class)
  predicted_data <- knn(train_data[, -8], test_data[, -8], train_data$SOURCE, k)
  
  all_fold_predicted_data <- c(all_fold_predicted_data, predicted_data)
  all_labels <- c(all_labels, test_data$SOURCE)
  accuracy[fold] <- sum(predicted_data == test_data$SOURCE) / nrow(test_data)
}

mean_accuracy <- mean(accuracy) * 100
print(paste(" 10-fold cross-validation Accuracy:", mean_accuracy, "%"))


split_data <- sample(1:NROW(normdata), size=NROW(normdata)*0.7)
my_train <- normdata[split_data,]
my_test <-normdata[-split_data,]
str(my_train)
str(my_test)


install.packages("class")
library(class)
knn_1 <-knn(train=my_train[,-8], test=my_test[,-8], cl=as.factor(my_train$SOURCE), k=5)
knn_1
accuracy1 <- 100* sum(my_test$SOURCE == knn_1)/NROW(my_test$SOURCE)
print(paste("Accuracy:", accuracy1 , "%"))

knn_2 <- knn(train = my_train[,-8], test = my_test[,-8], cl = my_train$SOURCE, k = 50)
knn_2
accuracy2 <- 100* sum(my_test$SOURCE == knn_2)/NROW(my_test$SOURCE)
print(paste("Accuracy:", accuracy2, "%"))



Confusion1= table(knn_1, my_test$SOURCE)
Confusion1
library(caret)
confusionMatrix(Confusion1)

Confusion2= table(knn_2, my_test$SOURCE)
Confusion2
library(caret)
confusionMatrix(Confusion2)


confusion_matrix <- confusionMatrix(table(knn_1, my_test$SOURCE))
recall <- confusion_matrix$byClass["Recall"]
precision <- confusion_matrix$byClass["Precision"]
recall
precision

confusion_matrix <- confusionMatrix(table(knn_2, my_test$SOURCE))
recall <- confusion_matrix$byClass["Recall"]
precision <- confusion_matrix$byClass["Precision"]
recall
precisions




