#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all femur data
setwd("//Users/Carrie/Desktop/")
femurdataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_femur.txt", header=T, sep="\t")

#Now we want only the "Human" data
#use grep function to pull out labels with Human in beginning of character string
Human.rows <- grep("^SBUhuman_", as.matrix(femurdataframe$label))

#Additional code for humans to include Point Hope (currently not working for femur)
PH.rows <- grep("^PointHope_", as.matrix(femurdataframe$label))
combined <- c(Human.rows, PH.rows)

#now pull out only those row levels which included Human
Human.femurdataframe <- femurdataframe[combined,]


#Create rows for J and Imin/Imax
Human.femurdataframe$J <- Human.femurdataframe$Imin + Human.femurdataframe$Imax
Human.femurdataframe$ratio <- Human.femurdataframe$Imax / Human.femurdataframe$Imin


#dataframe must be attached for this function to work
data <- Human.femurdataframe
attach(data)
