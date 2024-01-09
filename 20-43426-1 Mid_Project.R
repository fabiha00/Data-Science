mydata<- read.csv('D:/data.csv', header = TRUE , sep = ",")
mydata
options(max.print=99999)


names(mydata)
str(mydata)

summary(mydata)

typeof(mydata$Gender)
typeof(mydata$age)
typeof(mydata$parch)
typeof(mydata$fare)
typeof(mydata$embarked)
typeof(mydata$class)
typeof(mydata$who)
typeof(mydata$alone)
typeof(mydata$survived)



colSums(is.na(mydata))
mydata$class[mydata$class == ""] <- NA
colSums(is.na(mydata))

mydata$who[mydata$who == "womann" | mydata$who == "mannn" | mydata$who == "womannn"] <- NA
colSums(is.na(mydata))

mydata$Gender<-factor(mydata$Gender, levels=c(0,1),labels = c("male","female"))
mydata

mydata<-na.omit(mydata)
mydata


install.packages("dplyr")
library("dplyr")
sample_n(mydata,35)

mean <- mean(mydata$age, na.rm = TRUE)
mean
mydata$age[is.na(mydata$age)] <- mean
mydata
mean <- mean(mydata$Gender, na.rm = TRUE)
mean
mydata$Gender[is.na(mydata$Gender)] <- mean
mydata

mydata$class <- factor(mydata$class, levels = c("First","Second","Third"), labels = c(1,2,3))
mydata
mydata$class = as.integer(mydata$class)
mean <- mean(mydata$class, na.rm = TRUE)
mean
mydata$class[is.na(mydata$class)] <- mean
mydata

mydata$who <- factor(mydata$who, levels = c("man","woman","child"), labels = c(0,1,2))
mydata
mydata$who = as.integer(mydata$who)
mean <- mean(mydata$who, na.rm = TRUE)
mean
mydata$who[is.na(mydata$who)] <- mean
mydata
mean <- median(mydata$fare, na.rm = TRUE)
mean
mydata$fare[is.na(mydata$fare)] <- mean
mydata


median <- median(mydata$Gender, na.rm = TRUE)
median
mydata$Gender[is.na(mydata$Gender)] <- median
mydata

median <- median(mydata$age, na.rm = TRUE)
median
mydata$age[is.na(mydata$age)] <- median
mydata

median <- median(mydata$class, na.rm = TRUE)
median
mydata$class[is.na(mydata$class)] <- median
mydata

median <- median(mydata$who, na.rm = TRUE)
median
mydata$who[is.na(mydata$who)] <- median
mydata

mode <- names(sort(-table(mydata$Gender))) [1]
mode
mydata$Gender[is.na(mydata$Gender)] <- mode
mydata

mode <- names(sort(-table(mydata$age))) [1]
mode
mydata$age[is.na(mydata$age)] <- mode
mydata

mode <- names(sort(-table(mydata$class))) [1]
mode
mydata$class[is.na(mydata$class)] <- mode
mydata

mode <- names(sort(-table(mydata$who))) [1]
mode
mydata$who[is.na(mydata$who)] <- mode
mydata


hist(mydata$age)
hist(mydata$fare)

mydata$age[mydata$age > 70] = NA
mydata
mydata$fare[ mydata$fare > 250] = NA
mydata
mydata$fare[ mydata$fare < 2 ] = NA
mydata

boxplot(mydata$fare)
mydata$fare[ mydata$fare > 200] = NA
mydata
median <- median(mydata$fare, na.rm = TRUE)
median
mydata$fare[is.na(mydata$fare)] <- median
mydata 


mydata$age = as.integer(mydata$age)
range(mydata$age, na.rm = TRUE)                              
range <- max(mydata$age)-min(mydata$age)
range
s1 <- (mydata$age)
sd(s1)

range(mydata$fare, na.rm = TRUE)                              
range <- max(mydata$fare)-min(mydata$fare)
range
s2 <- (mydata$fare)
sd(s2)


mean <- mean(mydata$age, na.rm = TRUE)
mean
mean <- mean(mydata$Gender, na.rm = TRUE)
mean

median <- median(mydata$Gender, na.rm = TRUE)
median
median <- median(mydata$age, na.rm = TRUE)
median

mode <- names(sort(-table(mydata$Gender))) [1]
mode
mode <- names(sort(-table(mydata$age))) [1]
mode
