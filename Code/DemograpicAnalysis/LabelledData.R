

labelledData <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/Dataset/labelled_Data/lab_med_diet_new.csv',sep=",", header=TRUE)
head(labelledData)
length(unique(labelledData$SEQN))


LabslabelledData <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/Dataset/labelled_Data/labs_new.csv',sep=",", header=TRUE)
head(LabslabelledData)
length(unique(LabslabelledData$SEQN))
ncol(LabslabelledData)
LabslabelledData<-LabslabelledData[,c(2,22)]

MedicationlabelledData <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/Dataset/labelled_Data/medication_new.csv',sep=",", header=TRUE)
head(MedicationlabelledData)
length(unique(MedicationlabelledData$SEQN))
MedicationlabelledData<-MedicationlabelledData[,c(2,3)]

LabsMEd <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/Dataset/labelled_Data/lab_med_new.csv',sep=",", header=TRUE)
head(LabsMEd)
length(unique(LabsMEd$SEQN))

length(unique(cleanDemographicData$SEQN))

Examiniation <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/Dataset/labelled_Data/examination_new.csv',sep=",", header=TRUE)
head(Examiniation)
length(unique(Examiniation$SEQN))

DemDietLabsMed<-merge(cleanDemographicData, labelledData, by='SEQN', all.y=TRUE)
length(unique(DemDietLabsMed$SEQN))

#############################33

DemMed<-merge(cleanDemographicData, MedicationlabelledData, by='SEQN', all.y=TRUE)
length(unique(DemMed$SEQN))

#####
##Making Labelled Data
FinalLabels<-merge(MedicationlabelledData, LabslabelledData, by='SEQN', all=TRUE)
head(FinalLabels)
nrow(FinalLabels)
length(unique(FinalLabels$SEQN))

##Medication Data has 4k rows and is taken as the final value, For values which is zero in medical data
# we change to 1 if an only if Labs Labelled data is 1


which(FinalLabels$final_IsDia==0)
length(which(FinalLabels$final_IsDia==1))
toChange<-which(FinalLabels$Is_Diabetic==1)
FinalLabels$Is_Diabetic[toChange]
FinalLabels$final_IsDia[toChange]<-FinalLabels$Is_Diabetic[toChange]

#####


LabsOriginal <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/Dataset/original/national-health-and-nutrition-examination-survey/labs.csv',sep=",", header=TRUE)
head(LabsOriginal)
length(unique(LabsOriginal$SEQN))



MedicationOriginal <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/Dataset/original/national-health-and-nutrition-examination-survey/medications.csv',sep=",", header=TRUE)
head(MedicationOriginal)
length(unique(MedicationOriginal$SEQN))
  
 

MedicationInt<- MedicationOriginal[grepl("E", MedicationOriginal$RXDRSC1),]
length(unique(MedicationInt$SEQN)) 

