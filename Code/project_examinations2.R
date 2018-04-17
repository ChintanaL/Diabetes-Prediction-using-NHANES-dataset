examination <- read.csv("C:/Users/Lenovo-pc/Downloads/examination.csv")
examination <- read.csv("C:/Users/Lenovo-pc/Downloads/examination.csv", na.strings="",header=TRUE)
na.strings=c(""," ","NA")
View(examination)
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

examination_new$`Grip Test Stat` <- NULL
examination_new$`Dominant Hand` <- NULL
examination_new$`Combined Grip`<- NULL
examination_new$`Grip Strength H1`<- NULL
examination_new$`Grip Strength H2`<- NULL

View(examination_new)

library(foreach)
library(iterators)
library(itertools)

library(randomForest)
library(missForest)

examination_new <- missForest(examination_new)

View(examination_new$ximp)
#for(i in 1:ncol(examination_new)){
#  examination_new[is.na(examination_new[,i]), i] <- mean(examination_new[,i], na.rm = TRUE)
#}
examination_new$ximp$`CC Surface Condition` <- examination_row

examination_new$ximp$`CC Surface Condition` <- NULL
View(examination_new$ximp)

examination_new$ximp <- format(round(examination_new$ximp,2), nsmall = 2)

y <- examination_new$ximp
View(y)

#to remove .00 in the sequence coloumn
y <-transform(y, SEQN=as.numeric(SEQN))
View(q)

NCOL(examination_new$ximp)

write.csv(examination_new$ximp, "examination_full.csv")

l <- read.csv("C:/Users/Lenovo-pc/Downloads/finalLabelsE10.csv") 
View(l)

install.packages('plyr')
library(plyr)

#join(df1, df2,
#     type = "inner")

x <- inner_join(y, l)
View(x)


