#DATA CLEANING


#MEDICATION.CSV


medication <- read.csv("medications.csv")
medication[medication== ""] <- NA
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

medication_new<- completeFun(medication,"RXDDRUG")
nrow(medication_new)
View(medication_new)
medication_new<- completeFun(medication_new,"RXDDRGID") 
nrow(medication_new)
View(medication_new)
medication_new<- completeFun(medication_new,"RXDRSD1") 
nrow(medication_new)
medication_new <- subset(medication_new, select = -RXDRSC2)
View(medication_new)
medication_new <- subset(medication_new, select = -RXDRSC3)
medication_new <- subset(medication_new, select = -RXDRSD2)
medication_new <- subset(medication_new, select = -RXDRSD3)
nrow(medication_new)
View(medication_new)
medication_new["Is_Diabetic"] <- NA
medication_new$Is_Diabetic  <- ifelse(medication_new$RXDRSC1 == "E11", "Yes", "No")
library('plyr')
count(medication_new,'Is_Diabetic')



#LABS.CSV


labs <- read.csv("labs.csv")
install.packages("SASxport")
install.packages("Hmisc")
library(Hmisc)
library(SASxport)
lookup.xport("PBCD_H.XPT")
labs1 <- read.xport("PBCD_H.XPT")
lookup.xport("TCHOL_H.XPT")
labs2 <- read.xport("TCHOL_H.XPT")
lookup.xport("CBC_H.XPT")
labs3 <- read.xport("CBC_H.XPT")
lookup.xport("GHB_H.XPT")
labs4 <- read.xport("GHB_H.XPT")
lookup.xport("INS_H.XPT")
labs5 <- read.xport("INS_H.XPT")
lookup.xport("OGTT_H.XPT")
labs6<- read.xport("OGTT_H.XPT")
labs1_new <- subset(labs1, select = c (SEQN,LBXBCD,LBXBPB,LBXBMN,LBXTHG,LBXBSE))
labs2_new <- subset(labs2, select = c(SEQN,LBXTC))
labs3_new <- subset(labs3, select = c(SEQN,LBDLYMNO,LBDMONO,LBDNENO,LBDEONO,LBDBANO,LBXRBCSI,LBXHGB,LBXPLTSI))
labs5_new <- subset(labs5, select = c(SEQN,LBXIN,PHAFSTHR))
labs6_new <- subset(labs6, select = c(SEQN,LBXGLT,GTXDRANK))
labs_new <-merge(labs1_new,labs2_new)
labs_new <- merge(labs_new,labs3_new)
labs_new <- merge(labs_new,labs4)
labs_new <- merge(labs_new,labs5_new)
labs_new <- merge(labs_new,labs6_new)
for(i in 1:ncol(labs_new)){
  labs_new[is.na(labs_new[,i]), i] <- mean(labs_new[,i], na.rm = TRUE)
}
labs_new["Is_Diabetic"] <- NA
labs_new$Is_Diabetic  <- ifelse(labs_new$LBXGH >= 6.0, "Yes", "No")
write.csv(labs_new,'labs_new.csv')
#final_file <- join(medication_new,labs_new, by = Is_diabetic, type = "full", match = "all")
final_file <- merge(labs_new,medication_new)


#DIET.CSV

install.packages("SASxport")
install.packages("Hmisc")
library(Hmisc)
library(SASxport)
setwd("D:/DPA-Project")
lookup.xport("DSQTOT_H.xpt")
a<-read.xport("DSQTOT_H.xpt")
write.csv(a,"dietary.csv")
lookup.xport("DSQIDS_H.xpt")
b<-read.xport("DSQIDS_H.xpt")
write.csv(b,"dietary1.csv")
c<-merge(a,b)
write.csv(c,"FINAL_DIET_FILE.csv")
FINAL_DIET_FILE <- read.csv("D:/DPA-Project/FINAL_DIET_FILE.csv")
View(FINAL_DIET_FILE)
FINAL_DIET_FILE<-subset(FINAL_DIET_FILE,select=c(SEQN,DSDCOUNT,DSDANCNT,DSD010,DSD010AN,DSQIKCAL,DSQIPROT,DSQICARB,DSQISUGR,DSQIFIBE,DSQITFAT,DSQISFAT,DSQIMFAT,DSQIPFAT,DSQICHOL,DSQILYCO,DSQILZ,DSQIVB1,DSQIVB2,DSQINIAC,DSQIVB6,DSQIFA,DSQIFDFE,DSQICHL,DSQIVB12,DSQIVC,DSQIVK,DSQIVD,DSQICALC,DSQIPHOS,DSQIMAGN,DSQIIRON,DSQIZINC,DSQICOPP,DSQISODI,DSQIPOTA,DSQISELE,DSQICAFF,DSQIIODI))
completeFun(FINAL_DIET_FILE,C("DSQIKCAL","DSQIPROT","DSQICARB","DSQISUGR","DSQIFIBE","DSQITFAT","DSQISFAT","DSQIMFAT","DSQIPFAT","DSQICHOL","DSQILYCO","DSQILZ","DSQIVB1","DSQIVB2","DSQINIAC","DSQIVB6","DSQIFA","DSQIFDFE","DSQICHL","DSQIVB12","DSQIVC","DSQIVK","DSQIVD","DSQICALC","DSQIPHOS","DSQIMAGN","DSQIIRON","DSQIZINC","DSQICOPP","DSQISODI","DSQIPOTA","DSQISELE","DSQICAFF","DSQIIODI"))

FINAL_DIET_FILE[colSums(!is.na(FINAL_DIET_FILE))>0]
for(i in 1:ncol(FINAL_DIET_FILE)){
  data[is.na(FINAL_DIET_FILE[,i]), i] <- round(mean(FINAL_DIET_FILE[,i], na.rm = TRUE))
}
FINAL_DIET_FILE$DSQILYCO<-round(FINAL_DIET_FILE$DSQILYCO)
FINAL_DIET_FILE$DSQICAFF<-round(FINAL_DIET_FILE$DSQICAFF)
head(FINAL_DIET_FILE)
cols<-names(FINAL_DIET_FILE)[1:39]
library(dplyr)
d<-FINAL_DIET_FILE %>% mutate_each_(funs(round(.,1)), cols)
write.csv(d,"FINAL_DIET_FILE.csv")





