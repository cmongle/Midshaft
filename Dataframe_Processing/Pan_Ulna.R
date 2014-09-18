#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all ulna data
setwd("//Users/Carrie/Desktop/")
ulnadataframe <- read.table("Ape.Midshaft_Slice.Geometry.Output_ulna.txt", header=T, sep="\t")


#Now we want only the chimpanzee data
#use grep function to pull out labels with Pan in beginning of character string
pan.rows <- grep("^Pan_", as.matrix(ulnadataframe$label))
#now pull out only those row levels which included pan
pan.ulnadataframe <- ulnadataframe[pan.rows,]

pan.ulnadataframe$J <- pan.ulnadataframe$Imin + pan.ulnadataframe$Imax
pan.ulnadataframe$ratio <- pan.ulnadataframe$Imax / pan.ulnadataframe$Imin


#dataframe must be attached for this function to work
data <- (pan.ulnadataframe)
attach(data)