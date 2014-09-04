#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all humerus data
setwd("//Users/Carrie/Desktop/")
humerusdataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_humerus.txt", header=T, sep="\t")

#Now we want only the gorilla gorilla gorilla data
#Pull out the specific specimens (by number-- in measurement list) which are gorilla gorilla gorilla

gorilla.1 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_167337_Humerus",]
gorilla.2 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_167339_Humerus",]
gorilla.3 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_167335_Humerus",]
gorilla.4 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_201460_Humerus",]
gorilla.5 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_90289_Humerus",]
gorilla.6 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_167338_Humerus",]
gorilla.7 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_90290_Humerus",]
gorilla.8 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_81652_Humerus",]
gorilla.9 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_167340_Humerus",]
gorilla.10 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_54327_Humerus",]
gorilla.11 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_54356",]
gorilla.12 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_AMNH_54355",]
gorilla.13 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_20043_Humerus",]
gorilla.14 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_26850_Humerus",]
gorilla.15 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_29048_Humerus",]
gorilla.16 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_23162_Humerus",]
gorilla.17 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_38326_Humerus",]
gorilla.18 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_37264_Humerus",]
gorilla.19 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_57482_Humerus",]
gorilla.20 <- humerusdataframe[humerusdataframe[,2] == "Gorilla_MCZ_29049_Humerus",]


gorilla.humerusdataframe <- rbind(gorilla.1, gorilla.2, gorilla.3, gorilla.4, gorilla.5, gorilla.6, gorilla.7, gorilla.8, gorilla.9, gorilla.10, gorilla.11, gorilla.12, gorilla.13, gorilla.14, gorilla.15, gorilla.16, gorilla.17, gorilla.18, gorilla.19, gorilla.20)

#Create rows for J and Imin/Imax
gorilla.humerusdataframe$J <- gorilla.humerusdataframe$Imin + gorilla.humerusdataframe$Imax
gorilla.humerusdataframe$ratio <- gorilla.humerusdataframe$Imax / gorilla.humerusdataframe$Imin


#dataframe must be attached for this function to work

data <- gorilla.humerusdataframe
attach(data)
