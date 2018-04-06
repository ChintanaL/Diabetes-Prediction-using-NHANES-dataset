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
examination_new <- subset(examination, select=c("PEASCST1",
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
                                                "OHX02SE"
                                                ))
View(examination_new)
NCOL(examination_new)
examination_new_2 <- na.locf(examination_new$OHX02CTC)
examination_new$OHX02CTC <- NULL
for(i in 1:ncol(examination_new)){
  examination_new[is.na(examination_new[,i]), i] <- mean(examination_new[,i], na.rm = TRUE)
}
examination_new <- format(round(examination_new,2), nsmall = 2)
examination_new$OHX02CTC <- examination_new_2
View(examination_new)

View(examination_new)
is.na(examination_new)
