examination <- read.csv("C:/Users/Lenovo-pc/Downloads/examination.csv")
examination <- read.csv("C:/Users/Lenovo-pc/Downloads/examination.csv", na.strings="",header=TRUE)
na.strings=c(""," ","NA")
View(examination)
as.numeric(examination)
is.numeric(examination)
ncol(examination)
install.packages(dplyr)
library(dplyr)
library(zoo)
library(xts)
#blood pressure, body measure, femur, frax score, spine, vertebral fracture, grip test
#dentition, periodontal, 
#reccomendation of cure, taste&smell.
#subset(mtcars, select = c("mpg", "cyl", "vs", "am"))
#dat <- data.frame(A=c(1,2),B=c(3,4),C=c(5,6),D=c(7,7),E=c(8,8),F=c(9,9)) 
#> subset(dat, select=c("A", "B"))
examination_new <- subset(examination, select=c("SEQN",
                                                "PEASCST1",
                                                "BPXSY1",
                                                "BPXDI2",
                                                "BMXWT",
                                                "BMXRECUM",
                                                "BMXHT",
                                                "BMXHEAD",
                                                "BMXBMI",
                                                "BMXARMC",
                                                "BMXWAIST",
                                                "BMXSAD1",
                                                "BMDAVSAD",
                                                "MGDEXSTS",
                                                "MGD130",
                                                "MGDSEAT",
                                                "MGDCGSZ",
                                                "MGXH1T1",
                                                "MGXH2T1",
                                                "OHDEXSTS",
                                                "OHDDESTS",
                                                "OHXIMP",
                                                "OHX01TC",
                                                "OHX02CTC",
                                                "OHX02CSC",
                                                "OHX02SE"))
View(examination_new)
str(examination_new)
colnames(examination_new) <-  c("SEQN",
                                "BP Stat",
                                "BP Systolic",
                                "BP Dystolic",
                                "Weight",
                                "Recumbent Length",
                                "Height",
                                "Head Circum",
                                "BMI",
                                "Arm Circum",
                                "Waist Circum",
                                "SAD",
                                "Avg SAD",
                                "Grip Test Stat",
                                "Dominant Hand",
                                "Testing Position",
                                "Combined Grip",
                                "Grip Strength H1",
                                "Grip Strength H2",
                                "Overall Oral",
                                "Dentition Stat Code",
                                "Dental Implant",
                                "Tooth COunt",
                                "CC Surface Condition",
                                "Dental Sealants",
                                "Coronal Caries Tooth Count")                                              
View(examination_new)
str(examination_new)
is.numeric(examination_new)
examination_new$`Grip Test Stat` <- NULL
examination_new$`Dominant Hand` <- NULL
examination_new$`Combined Grip`<- NULL
examination_new$`Grip Strength H1`<- NULL
examination_new$`Grip Strength H2`<- NULL
examination_new$`CC Surface Condition` <- NULL
str(examination_new)
View(examination_new)

library(foreach)
library(iterators)
library(itertools)

library(randomForest)
library(missForest)

examination_new <- missForest(examination_new)
str(examination_new$ximp)
View(examination_new$ximp)
ncol(examination_new$ximp)
i <- as.numeric(examination_new$ximp$`BP Stat`)
i1 <- as.numeric(examination_new$ximp$`BP Systolic`)
i2 <- as.numeric(examination_new$ximp$`BP Dystolic`)
i3 <- as.numeric(examination_new$ximp$Weight)
i4 <- as.numeric(examination_new$ximp$`Recumbent Length`)
i5 <- as.numeric(examination_new$ximp$Height)
i6 <- as.numeric(examination_new$ximp$`Head Circum`)
i7 <- as.numeric(examination_new$ximp$BMI)
i8 <- as.numeric(examination_new$ximp$`Arm Circum`)
i9 <- as.numeric(examination_new$ximp$`Waist Circum`)
i10 <- as.numeric(examination_new$ximp$SAD)
i11 <- as.numeric(examination_new$ximp$`Avg SAD`)
i12 <- as.numeric(examination_new$ximp$`Testing Position`)
i13 <- as.numeric(examination_new$ximp$`Overall Oral`)
i14 <- as.numeric(examination_new$ximp$`Dentition Stat Code`)
i15 <- as.numeric(examination_new$ximp$`Dental Implant`)
i16 <- as.numeric(examination_new$ximp$`Tooth COunt`)
i17 <- as.numeric(examination_new$ximp$`Dental Sealants`)
i18 <- as.numeric(examination_new$ximp$`Coronal Caries Tooth Count`)
i19 <- as.numeric(examination_new$ximp$SEQN)
s <- cbind(i19, i, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18)
colnames(s) <-  c("SEQN",
                                "BP Stat",
                                "BP Systolic",
                                "BP Dystolic",
                                "Weight",
                                "Recumbent Length",
                                "Height",
                                "Head Circum",
                                "BMI",
                                "Arm Circum",
                                "Waist Circum",
                                "SAD",
                                "Avg SAD",
                                "Testing Position",
                                "Overall Oral",
                                "Dentition Stat Code",
                                "Dental Implant",
                                "Tooth COunt",
                                "Dental Sealants",
                                "Coronal Caries Tooth Count")       
View(s)
str(s)
is.numeric(s)
#for(i in 1:ncol(examination_new)){
#  examination_new[is.na(examination_new[,i]), i] <- mean(examination_new[,i], na.rm = TRUE)
#}
#examination_new$ximp$`CC Surface Condition` <- examination_row

#examination_new$ximp$`CC Surface Condition` <- NULL
#View(examination_new$ximp)

examination_new$ximp <- format(round(examination_new$ximp,2), nsmall = 2)
str(examination_new)
s <- examination_new$ximp
s<- list(s)
as.numeric(s)
View(s)
str(s)

#to remove .00 in the sequence coloumn
#s <-transform(s, SEQN=as.numeric(SEQN))
#View(s)

NCOL(examination_new$ximp)

#write.csv(examination_new$ximp, "finalLabels_18thApril_allE")

a <- read.csv("C:/Users/Lenovo-pc/Downloads/finalLabels_18thApril_allE.csv") 
View(a)
str(a)
b <- as.numeric(a$X)
b1 <- as.numeric(a$SEQN)
b2 <- as.numeric(a$is_diabetic)
a <- cbind(b, b1, b2)
colnames(a) <- c("X", "SEQN", "is_diabetic")
View(a)
is.numeric(a)
install.packages('plyr')
library(plyr)

#join(df1, df2,
#     type = "inner")

x <- merge(s, a, by="SEQN")

View(x)
str(x)
is.numeric(x)
summary(x)

names<-colnames(x)
names
names_new <- names[-22]
names_new
#vars <- c("Sepal.Length", "Sepal.Width", "Petal.Width")
#all <- lapply(vars, function(i) list(x= iris[,i], y=iris[,"Petal.Length"]))
#lapply(all, function(x) do.call(cor, x))
all <- lapply(names_new, function(i) list(x= x[,i], y=x[,"is_diabetic"]))

lapply(all, function(x) do.call(cor, x))
names(cor_var) <- names_new
cor_var_new <- sort(unlist(cor_var), decreasing=TRUE) 
cor_var_new
chisq.test(table(x$`BP Stat`, x$is_diabetic))
chisq.test(table(x$`BP Systolic`, x$is_diabetic))
chisq.test(table(x$`BP Dystolic`, x$is_diabetic))
chisq.test(table(x$Weight, x$is_diabetic))
chisq.test(table(x$`Recumbent Length`, x$is_diabetic))
chisq.test(table(x$Height, x$is_diabetic))
chisq.test(table(x$`Head Circum`, x$is_diabetic))
chisq.test(table(x$BMI, x$is_diabetic))
chisq.test(table(x$`Arm Circum`, x$is_diabetic))
chisq.test(table(x$`Waist Circum`, x$is_diabetic))
chisq.test(table(x$SAD, x$is_diabetic))
chisq.test(table(x$`Avg SAD`, x$is_diabetic))
chisq.test(table(x$`Testing Position`, x$is_diabetic))
chisq.test(table(x$`Overall Oral`, x$is_diabetic))
chisq.test(table(x$`Dentition Stat Code`, x$is_diabetic))
chisq.test(table(x$`Dental Implant`, x$is_diabetic))
chisq.test(table(x$`Tooth COunt`, x$is_diabetic))
chisq.test(table(x$`Dental Sealants`, x$is_diabetic))
chisq.test(table(x$`Coronal Caries Tooth Count`, x$is_diabetic))
chisq.test(table(x$X, x$is_diabetic))
#LR
model <- glm(is_diabetic~.,family=binomial(link = 'logit'), data = x)
summary(model)
bp_stat <- x$`BP Stat`
rcumbent_length <- x$`Recumbent Length`
avg_sad <- x$`Avg SAD`
dental_sealants <- x$`Dental Sealants`
x$`BP Stat`<- NULL
x$`Recumbent Length`<- NULL
x$`Avg SAD`<- NULL
x$`Dental Sealants` <- NULL
x$`Tooth COunt`<- NULL
x$`Testing Position` <- NULL
x$`Overall Oral`<- NULL
x$`Dental Implant`<- NULL
x$`Dentition Stat Code` <- NULL
x$`Coronal Caries Tooth Count` <- NULL
View(x)
ncol(x)
write.csv(x, "examinations_complete2.csv")
str(x)
