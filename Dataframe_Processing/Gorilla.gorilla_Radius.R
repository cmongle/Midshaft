#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all radius data
setwd("//Users/Carrie/Desktop/")
radiusdataframe <- read.table("Ape.Midshaft_Slice.Geometry.Output_radius.txt", header=T, sep="\t")

#Now we want only the gorilla gorilla gorilla data
#Pull out the specific specimens (by number-- in measurement list) which are gorilla gorilla gorilla

gorilla.1 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_167337_Radius",]
gorilla.2 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_167339_Radius",]
gorilla.3 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_167335_Radius",]
gorilla.4 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_201460_Radius.tif",]
gorilla.5 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_90289_Radius",]
gorilla.6 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_167338_Radius",]
gorilla.7 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_90290_Radius",]
gorilla.8 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_81652_Radius",]
gorilla.9 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_167340_Radius",]
gorilla.10 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_54327_Radius",]
gorilla.11 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_54356_Radius",]
gorilla.12 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_AMNH_54355_Radius",]
gorilla.13 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_20043_Radius",]
gorilla.14 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_26850_Radius",]
gorilla.15 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_29048_Radius",]
gorilla.16 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_23162_Radius",]
gorilla.17 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_38326_Radius",]
gorilla.18 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_37264_Radius",]
gorilla.19 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_57482_Radius",]
gorilla.20 <- radiusdataframe[radiusdataframe[,2] == "Gorilla_MCZ_29049_Radius",]

gorilla.radiusdataframe <- rbind(gorilla.1, gorilla.2, gorilla.3, gorilla.4, gorilla.5, gorilla.6, gorilla.7, gorilla.8, gorilla.9, gorilla.10, gorilla.11, gorilla.12, gorilla.13, gorilla.14, gorilla.15, gorilla.16, gorilla.17, gorilla.18, gorilla.19, gorilla.20)
#Create rows for J and Imin/Imax
gorilla.radiusdataframe$J <- gorilla.radiusdataframe$Imin + gorilla.radiusdataframe$Imax
gorilla.radiusdataframe$ratio <- gorilla.radiusdataframe$Imax / gorilla.radiusdataframe$Imin


data <- gorilla.radiusdataframe
attach(data)
