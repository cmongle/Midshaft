#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all tibia data
setwd("//Users/Carrie/Desktop/")
Tibiadataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_tibia.txt", header=T, sep="\t")

#Now we want only the gorilla gorilla gorilla data
#Pull out the specific specimens (by number-- in measurement list) which are gorilla gorilla gorilla

gorilla.1 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_167337_Tibia",]
gorilla.2 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_167339_Tibia",]
gorilla.3 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_167335_Tibia",]
gorilla.4 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_201460_Tibia",]
gorilla.5 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_90289_Tibia",]
gorilla.6 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_167338_Tibia",]
gorilla.7 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_90290_Tibia",]
gorilla.8 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_81652_Tibia",]
gorilla.9 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_167340_Tibia",]
gorilla.10 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_54327_Tibia",]
gorilla.11 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_54356_Tibia",]
gorilla.12 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_AMNH_54355_Tibia",]
gorilla.13 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_20043_Tibia",]
gorilla.14 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_26850_Tibia",]
gorilla.15 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_29048_Tibia",]
gorilla.16 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_23162_Tibia",]
gorilla.17 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_38326_Tibia",]
gorilla.18 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_37264_Tibia",]
gorilla.19 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_57482_Tibia",]
gorilla.20 <- Tibiadataframe[Tibiadataframe[,2] == "Gorilla_MCZ_29049_Tibia",]


gorilla.tibiadataframe <- rbind(gorilla.1, gorilla.2, gorilla.3, gorilla.4, gorilla.5, gorilla.6, gorilla.7, gorilla.8, gorilla.9, gorilla.10, gorilla.11, gorilla.12, gorilla.13, gorilla.14, gorilla.15, gorilla.16, gorilla.17, gorilla.18, gorilla.19, gorilla.20)

#Create rows for J and Imin/Imax
gorilla.tibiadataframe$J <- gorilla.tibiadataframe$Imin + gorilla.tibiadataframe$Imax
gorilla.tibiadataframe$ratio <- gorilla.tibiadataframe$Imax / gorilla.tibiadataframe$Imin


#dataframe must be attached for this function to work

data <- gorilla.tibiadataframe
attach(data)

