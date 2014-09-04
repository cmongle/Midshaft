#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all ulna data
setwd("//Users/Carrie/Desktop/")
ulnadataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_ulna.txt", header=T, sep="\t")

ulnadataframe <- read.csv("//Users//Carrie/Documents/Stony Brook//Research Projects/2013-14_Great.Ape_Long.Bones/Great.Ape_CSV_Project.Data_Files/Ape.Midshaft_Slice.Geometry.Output_Ulna.csv")
#Now we want only the gorilla gorilla gorilla data
#Pull out the specific specimens (by number-- in measurement list) which are gorilla gorilla gorilla

gorilla.1 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_167337_Ulna",]
gorilla.2 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_167339_Ulna",]
gorilla.3 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_167335_Ulna",]
gorilla.4 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_201460_Ulna",]
gorilla.5 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_90289_Ulna",]
gorilla.6 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_167338_Ulna",]
gorilla.7 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_90290_Ulna",]
gorilla.8 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_81652_Ulna",]
gorilla.9 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_167340_Ulna",]
gorilla.10 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_54327_Ulna",]
gorilla.11 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_54356_Ulna",]
gorilla.12 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_AMNH_54355_Ulna",]
gorilla.13 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_20043_Ulna",]
gorilla.14 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_26850_Ulna",]
gorilla.15 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_29048_Ulna",]
gorilla.16 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_23162_Ulna",]
gorilla.17 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_38326_Ulna",]
gorilla.18 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_37264_Ulna",]
gorilla.19 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_57482_Ulna",]
gorilla.20 <- ulnadataframe[ulnadataframe[,2] == "Gorilla_MCZ_29049_Ulna",]

gorilla.ulnadataframe <- rbind(gorilla.1, gorilla.2, gorilla.3, gorilla.4, gorilla.5, gorilla.6, gorilla.7, gorilla.8, gorilla.9, gorilla.10, gorilla.11, gorilla.12, gorilla.13, gorilla.14, gorilla.15, gorilla.16, gorilla.17, gorilla.18, gorilla.19, gorilla.20)
#include only columns of interest in order to make analysis run faster
gorilla.ulnadataframe <- gorilla.ulnadataframe[,c(2,4,5,14,15)]

#dataframe must be attached for this function to work
data <- (gorilla.ulnadataframe)
attach(data)