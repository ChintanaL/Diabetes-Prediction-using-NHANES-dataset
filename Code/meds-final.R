#setwd("C:/Users/HP USER/Desktop/Spring 2018/MATH 571/Project")
medication <- read.csv("medications.csv")
medication[medication== ""] <- NA
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
medication_new<- completeFun(medication,"RXDDRUG")
medication_new<- completeFun(medication_new,"RXDDRGID")
medication_new<- completeFun(medication_new,"RXDRSD1")
medication_new<- completeFun(medication_new,"RXQSEEN")
medication_new<- completeFun(medication_new,"RXDRSC1")
medication_new<- completeFun(medication_new,"RXDCOUNT")
medication_new <- subset(medication_new, select = -RXDRSC2)
View(medication_new)
medication_new <- subset(medication_new, select = -RXDRSC3)
medication_new <- subset(medication_new, select = -RXDRSD2)
medication_new <- subset(medication_new, select = -RXDRSD3)
medication_new["Is_Diabetic"] <- NA
medication_new$Is_Diabetic  <- ifelse(medication_new$RXDRSC1 == "E11", 1 , 0)
medication_new <- subset(medication_new, select = c(SEQN,Is_Diabetic))


#meds <- medication_new%>%
 # select(SEQN,Is_Diabetic) %>%
  #group_by(SEQN) 
library(plyr)
meds <- ddply(medication_new,.(SEQN), summarize, Is_Diabetic = paste(Is_Diabetic, collapse = ","))
library(splitstackshape)
meds_1 <- cSplit(meds, "Is_Diabetic", ",")

library(tidyr)
library(dplyr)

meds_2 <- mutate(meds_1, final_IsDia = ifelse(Is_Diabetic_01 | Is_Diabetic_02 |Is_Diabetic_03 |Is_Diabetic_04 |Is_Diabetic_05 |Is_Diabetic_06 |Is_Diabetic_07|Is_Diabetic_08 |Is_Diabetic_09 |Is_Diabetic_10 |Is_Diabetic_11 |Is_Diabetic_12 |Is_Diabetic_13 |Is_Diabetic_14 |Is_Diabetic_15 |Is_Diabetic_16 |Is_Diabetic_17 |Is_Diabetic_18 |Is_Diabetic_19 |Is_Diabetic_20 |Is_Diabetic_20 |Is_Diabetic_21 |Is_Diabetic_22 |Is_Diabetic_23  == 1, 1 , 0))

meds_2 <- subset(meds_2, select = c(SEQN,final_IsDia))

meds_2[is.na(meds_2)] <- 0

View(meds_2)

write.csv(meds_2, 'medication_new.csv')


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
labs5<- read.xport("INS_H.XPT")
lookup.xport("OGTT_H.XPT")
labs6<- read.xport("OGTT_H.XPT")
labs1_new <- subset(labs1, select = c (SEQN,LBXBCD,LBXBPB,LBXBMN,LBXTHG,LBXBSE))
labs2_new <- subset(labs2, select = c(SEQN,LBXTC))
labs3_new <- subset(labs3, select = c(SEQN,LBDLYMNO,LBDMONO,LBDNENO,LBDEONO,LBDBANO,LBXRBCSI,LBXHGB,LBXPLTSI))
labs5_new <- subset(labs5, select = c(SEQN,LBXIN,PHAFSTHR))
labs6_new <- subset(labs6, select = c(SEQN,LBXGLT,GTXDRANK))
labs_new <- merge(labs1_new,labs2_new, by = "SEQN")
labs_new <- merge(labs_new,labs3_new, by = "SEQN")
labs_new <- merge(labs_new,labs4, by = "SEQN")
labs_new <- merge(labs_new,labs5_new, by = "SEQN")
labs_new <- merge(labs_new,labs6_new, by = "SEQN")
for(i in 1:ncol(labs_new)){
  labs_new[is.na(labs_new[,i]), i] <- mean(labs_new[,i], na.rm = TRUE)
}
labs_new["Is_Diabetic"] <- NA
labs_new$Is_Diabetic  <- ifelse(labs_new$LBXGH >= 6.0, 1 , 0)
write.csv(labs_new,'labs_new.csv')
