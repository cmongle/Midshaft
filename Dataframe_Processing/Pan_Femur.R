#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all femur data
setwd("//Users/Carrie/Desktop/")
femurdataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_femur.txt", header=T, sep="\t")


#Now we want only the chimpanzee data
#use grep function to pull out labels with Pan in beginning of character string
pan.rows <- grep("^Pan_", as.matrix(femurdataframe$label))
#now pull out only those row levels which included pan
pan.femurdataframe <- femurdataframe[pan.rows,]

pan.femurdataframe$J <- pan.femurdataframe$Imin + pan.femurdataframe$Imax
pan.femurdataframe$ratio <- pan.femurdataframe$Imax / pan.femurdataframe$Imin


#dataframe must be attached for this function to work
data <- (pan.femurdataframe)
attach(data)