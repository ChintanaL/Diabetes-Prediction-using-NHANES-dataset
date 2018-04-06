library(tidyr)
library(dplyr)

#DIET
diet <- read.csv("C:/Users/Lenovo-pc/Downloads/diet.csv")
str(diet)
View(diet)
diet[colSums(!is.na(diet))>0]
str(diet)
is.na(diet)
#mean in place of nas
for(i in 1:ncol(diet)){
  diet[is.na(diet[,i]), i] <- mean(diet[,i], na.rm = TRUE)
}

#another method
#library(zoo)
#na.aggregate(diet)
#NA2mean <- function(diet) replace(diet, is.na(diet), mean(diet, na.rm = TRUE))
#replace(diet, TRUE, lapply(diet, NA2mean))

summary(diet)
str(diet)
View(diet)
diet$DRQSDT5 <- NULL
View(diet)
#column with all Na's is eliminated

#EXAMINATION
examination<-read.csv("C:/Users/Lenovo-pc/Downloads/examination.csv")
examination <- read.csv("C:/Users/Lenovo-pc/Downloads/examination.csv", na.strings="",header=TRUE)
na.strings=c(""," ","NA")
#View(examination)
for(i in 1:ncol(examination)){
  examination[is.na(examination[,i]), i] <- mean(examination[,i], na.rm = TRUE)
}
examination <-examination[rowSums(is.na(examination)) !=ncol(examination),]
#exmination <- na.omit(examination)
View(examination)
NROW(examination)
#View(examination)
examination$BMIHEAD <- NULL
is.na(examination)
examination <- na.omit(examination)
NROW(examination)
View(examination)
str(examination)
View(examination)
#head(examination)
#tail(examination)
#str(examination)
#is.na(examination)

#DEMOGRAPHIC
demographic <- read.csv("C:/Users/Lenovo-pc/Downloads/demographic.csv")
View(demographic)
is.na(demographic)
#na.omit(demographic)
View(demographic)
for(i in 1:ncol(demographic)){
  demographic[is.na(demographic[,i]), i] <- mean(demographic[,i], na.rm = TRUE)
}
View(demographic)
is.na(demographic)
str(demographic)

#LABS
labs <- read.csv("C:/Users/Lenovo-pc/Downloads/labs.csv")
str(labs)
View(labs)
for(i in 1:ncol(labs)){
  labs[is.na(labs[,i]), i] <- mean(labs[,i], na.rm = TRUE)
}
View(labs)
is.na(labs)

#MEDICATIONS

medications <- read.csv("C:/Users/Lenovo-pc/Downloads/medications.csv")
#dat <- read.csv("data2.csv",na.strings=" ",header=TRUE)

# to fill nas in all the blank spaces
medications <- read.csv("C:/Users/Lenovo-pc/Downloads/medications.csv", na.strings="",header=TRUE)
na.strings=c(""," ","NA")
NROW(medications)
#View(medications)
#na.omit(medications)
View(medications)
#data[rowSums(is.na(data)) != ncol(data),]

#removing rows with all vales na
medications <- medications[rowSums(is.na(medications)) !=ncol(medications),]
View(medications)
NROW(medications)
#data[rowSums(is.na(data)) == 0,]
medications <- medications[rowSums(is.na(medications)) == 0,]
View(medications)
NROW(medications)
