#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all Femur data
setwd("//Users/Carrie/Desktop/")
Femurdataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_femur.txt", header=T, sep="\t")

#Now we want only the gorilla gorilla gorilla data
#Pull out the specific specimens (by number-- in measurement list) which are gorilla gorilla gorilla

gorilla.1 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_167337_Femur",]
gorilla.2 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_167339_Femur",]
gorilla.3 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_167335_Femur",]
gorilla.4 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_201460_Femur",]
gorilla.5 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_90289_Femur",]
gorilla.6 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_167338_Femur",]
gorilla.7 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_90290_Femur",]
gorilla.8 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_81652_Femur",]
gorilla.9 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_167340_Femur",]
gorilla.10 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_54327_Femur",]
gorilla.11 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_54356_Femur",]
gorilla.12 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_AMNH_54355_Femur",]
gorilla.13 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_20043_Femur",]
gorilla.14 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_26850_Femur",]
gorilla.15 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_29048_Femur",]
gorilla.16 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_23162_Femur",]
gorilla.17 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_38326_Femur",]
gorilla.18 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_37264_Femur",]
gorilla.19 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_57482_Femur",]
gorilla.20 <- Femurdataframe[Femurdataframe[,2] == "Gorilla_MCZ_29049_Femur",]


gorilla.Femurdataframe <- rbind(gorilla.1, gorilla.2, gorilla.3, gorilla.4, gorilla.5, gorilla.6, gorilla.7, gorilla.8, gorilla.9, gorilla.10, gorilla.11, gorilla.12, gorilla.13, gorilla.14, gorilla.15, gorilla.16, gorilla.17, gorilla.18, gorilla.19, gorilla.20)

#Create rows for J and Imin/Imax
gorilla.Femurdataframe$J <- gorilla.Femurdataframe$Imin + gorilla.Femurdataframe$Imax
gorilla.Femurdataframe$ratio <- gorilla.Femurdataframe$Imax / gorilla.Femurdataframe$Imin


#dataframe must be attached for this function to work

data <- gorilla.Femurdataframe
attach(data)

