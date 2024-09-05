dataset<-read.csv("C:/Group_1.csv", header=TRUE, sep=",")
options(max.print = 1000)
dataset

summary(dataset)


install.packages("dplyr")
library(dplyr)
dataset$Gender <- tolower(dataset$Gender)
dataset

invalid_rows <-dataset[!dataset$Gender %in% c("male", "female"),]
invalid_rows

dataset$Gender <- gsub("Mmale","male",dataset$Gender,ignore.case = TRUE)
dataset$Gender <- gsub("Feemale","female",dataset$Gender,ignore.case = TRUE)
dataset




is_numeric <- function(x) {
  !is.na(as.numeric(x))
}
invalid_values <- dataset[!sapply(dataset$weight.kg., is_numeric), "weight.kg."]
invalid_values


clean_numeric <- function(x) {
  
  as.numeric(gsub("[^0-9.]", "", x))
}
dataset$weight.kg. <- sapply(dataset$weight.kg., clean_numeric)
dataset



install.packages("tibble")
library("tibble")
dataset$Gender<- factor(dataset$Gender, levels=c("male","female"),
labels= c(1,2))
dataset

dataset$Blood<- factor(dataset$Blood, levels=c("high","normal","low"),
labels= c(3,2,1))
dataset



which(is.na(dataset$id))
which(is.na(dataset$Age))
which(is.na(dataset$Gender))
which(is.na(dataset$weight.kg.))
which(is.na(dataset$Delivery_number))
which(is.na(dataset$Delivery_time))
which(is.na(dataset$Blood))
which(is.na(dataset$Heart))
which(is.na(dataset$Caesarian))
colSums(is.na(dataset))
sum(is.na(dataset))
is.na(dataset)



install.packages("naniar")
library(naniar)
gg_miss_var(dataset, show_pct = TRUE)
gg_miss_var(dataset)
vis_miss(dataset)

missing_counts <- colSums(is.na(dataset))

barplot(missing_counts, 
        main = "Missing Values in Each Column", 
        
        ylab = "Number of Missing Values",
        col = "skyblue",
        las=2 )



remove <- na.omit(dataset)
remove

mean_age <- mean(dataset$Age, na.rm = TRUE)
mean_age
dataset$Age[is.na(dataset$Age)] <-mean_age
dataset$Age <- ceiling(dataset$Age)
dataset

median_age<-median(dataset$Age,na.rm=TRUE)
median_age
dataset$Age[is.na(dataset$Age)] <- median_age
dataset

install.packages("modeest")
library(modeest)
mode_age <- names(which.max(table(dataset$Age)))
mode_age
dataset$Age[is.na(dataset$Age)] <- mode_age
dataset

install.packages("zoo")
library(zoo)
topdown <- dataset$Age<-na.locf(dataset$Age)
dataset$Age[is.na(dataset$Age)]<-topdown
dataset

bottomup<-dataset$Age <- na.locf(dataset$Age, fromLast = TRUE)
dataset$Age[is.na(dataset$Age)]<-bottomup
dataset


mode_gender <- mlv(dataset$Gender,method="mfv")
mode_gender
dataset$Gender[is.na(dataset$Gender)] <- mode_gender
dataset


mean_weight <- mean(as.numeric(dataset$weight.kg.), na.rm = TRUE)
mean_weight
dataset$weight.kg.[is.na(dataset$weight.kg.)] <- mean_weight
dataset$weight.kg. <- ceiling(as.numeric(dataset$weight.kg.))
dataset

median_weight <- median(dataset$weight.kg.,na.rm=TRUE)
median_weight
dataset$weight.kg.[is.na(dataset$weight.kg.)] <- median_weight
dataset

mode_weight <- names(which.max(table(dataset$weight.kg.)))
mode_weight
dataset$weight.kg.[is.na(dataset$weight.kg.)] <- mode_weight
dataset


mode_deliverynumber <- mlv(dataset$Delivery_number,method="mfv")
mode_deliverynumber
dataset$Delivery_number[is.na(dataset$Delivery_number)] <- mode_deliverynumber
dataset


mode_deliverytime <- mlv(dataset$Delivery_time,method="mfv")
mode_deliverytime
dataset$Delivery_time[is.na(dataset$Delivery_time)] <- mode_deliverytime
dataset


mode_blood <- mlv(dataset$Blood,method="mfv")
mode_blood
dataset$Blood[is.na(dataset$Blood)] <- mode_blood
dataset

mode_heart <- mlv(dataset$Heart,method="mfv")
mode_heart
dataset$Heart[is.na(dataset$Heart)] <- mode_heart
dataset


mode_caesarian <- mlv(dataset$Caesarian,method="mfv")
mode_caesarian
dataset$Caesarian[is.na(dataset$Caesarian)] <- mode_caesarian
dataset





sd_age<-sd(dataset$Age);
sd_age
x <- seq(-100, 100, by =1) 
y <- dnorm(x,mean_age, sd_age) 
plot(x,y)


x <- seq(-100, 100, by =1) 
y <- dnorm(x,median_age, sd_age) 
plot(x,y)



mode_age
x <- seq(-100, 100, by =1) 
y <- dnorm(x,26, sd_age) 
plot(x,y)


dataset$Gender_n
sd_gender<-0.34;
sd_gender
x <- seq(-3,3, by =1) 
y <- dnorm(x,1,sd_gender) 
plot(x,y)


median_weight
sd_weight<-sd(dataset$weight.kg.);
sd_weight
x <- seq(0,100, by =1) 
y <- dnorm(x, mean_weight,sd_weight) 
plot(x,y)


x <- seq(0,100, by =1) 
y <- dnorm(x,median_weight,sd_weight) 
plot(x,y)


mode_weight
x <- seq(0,100, by =1) 
y <- dnorm(x,63,sd_weight) 
plot(x,y)


sdp<-sd(dataset$Delivery_number);
sdp
x <- seq(-3,10, by =1) 
y <- dnorm(x, mode_deliverynumber,sdp) 
plot(x,y)


sdp<-sd(dataset$Delivery_time);
sdp
x <- seq(-3,3, by =1) 
y <- dnorm(x, mode_deliverytime,sdp) 
plot(x,y)


sd_blood<- 9.38; 
sd_blood
x <- seq(-4,4, by =1) 
y <- dnorm(x,2,sd_blood) 
plot(x,y)


sdp<-sd(dataset$Heart);
sdp
x <- seq(-3,10, by =1) 
y <- dnorm(x,mode_heart,sdp) 
plot(x,y)



sdp<-sd(dataset$Caesarian);
sdp
x <- seq(-3,10, by =1) 
y <- dnorm(x,mode_caesarian,sdp) 
plot(x,y)




dataset$Delivery_time<- factor(dataset$Delivery_time, levels=c(0,1,2),
labels= c("timely","premature","latecomer"))
dataset

dataset$Heart<- factor(dataset$Heart, levels=c(0,1),
labels= c("apt","inept"))
dataset

dataset$Caesarian<- factor(dataset$Caesarian, levels=c(0,1),
labels= c("No","Yes"))
dataset




re<-na.omit(dataset)
re
x <- data.frame(step = c(re$Age)) 
normalized <- (x-min(x))/(max(x)-min(x))
normalized




outliers_age <- boxplot.stats(dataset$Age)$out
outliers_age

dataset$Age <- as.numeric(dataset$Age)
outliers_indices <- which(dataset$Age %in% boxplot.stats(dataset$Age)$out)
outliers_indices

outlier_clean <- dataset[-outliers_indices, ]
outlier_clean



outliers_weight<- boxplot.stats(dataset$weight.kg.)$out
outliers_weight
dataset


dataset$weight.kg. <- as.numeric(dataset$weight.kg.)
outliers_indices_w <- which(dataset$weight.kg. %in% boxplot.stats(dataset$weight.kg.)$out)
outliers_indices_w

outlier_clean_W <- dataset[-outliers_indices_w, ]
outlier_clean_W



outliers_delivary<- boxplot.stats(dataset$Delivery_number)$out
outliers_delivary

dataset$Delivery_number <- as.numeric(dataset$Delivery_number)
outliers_indices_dn <- which(dataset$Delivery_number %in% boxplot.stats(dataset$Delivery_number)$out)
outliers_indices_dn

outlier_clean <- dataset[-outliers_indices_dn, ]
outlier_clean

