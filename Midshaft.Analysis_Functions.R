
##########################################################################################
##########################################################################################
#CSA Analysis ####
# Function to pull out csa at intervals and compile into dataframe 
midshaft.csa.function <- function(x)
{
  slicelength <- as.numeric(length(csa[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slice.bind <- cbind(seq(1, slicelength), (csa[which(label == x)]))
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  csa.levels <- data.frame(rbind(slice.matrix[,2]))
  csa.levels <- cbind(x, csa.levels)
  colnames(csa.levels) <- c("specimen", seq(0,95, by=5))
  perdiff <- rbind(((csa.levels[,2:21] - csa.levels[,12])/csa.levels[,12])*100)/length(unique(x))
  perdiff <- data.frame(cbind(x, perdiff))
  colnames(perdiff) <- c("specimen",seq(0,95, by=5))
  return(perdiff)
}

x <- as.vector(data$label)
length(data$csa)
length(x)

result.csa = mdply(unique(x), midshaft.csa.function)


#Reshape into long format in order to plot 
result.csa.long <- melt(result.csa, variable.name="level", value.name="percent.difference")

result.csa.long <- rbind(result.csa.long[which(result.csa.long$level == 30),], 
                         result.csa.long[which(result.csa.long$level == 35),],
                         result.csa.long[which(result.csa.long$level == 40),],
                         result.csa.long[which(result.csa.long$level == 45),],
                         result.csa.long[which(result.csa.long$level == 50),],
                         result.csa.long[which(result.csa.long$level == 55),],
                         result.csa.long[which(result.csa.long$level == 60),],
                         result.csa.long[which(result.csa.long$level == 65),],
                         result.csa.long[which(result.csa.long$level == 70),])

#*****Need to work on this code to make a Repeated measures ANOVA####
#m <- aov(csa.result.diaphysis$percent.difference ~ csa.result.diaphysis$level)
#csa.m.tuk <- TukeyHSD(m)
#write.table(csa.m.tuk[1], file="Human_femur_aov_csa.txt", sep="\t")

#Pull out the mean percent differences from the TukeyHSD at each level of interest
#midshaft.aov <- c('50-30','50-35','50-40','50-45', '55-50', '60-50', '65-50','70-50')
#csa.midshaft.aov <- as.data.frame(csa.m.tuk$'csa.result.diaphysis$level'[midshaft.aov,])
#csa.midshaft.aov['50-50',] <- 0
#csa.midshaft.aov$level <- c(30, 35, 40, 45, 55, 60, 65, 70, 50)
#csa.midshaft.aov <- abs(csa.midshaft.aov)
#csa.midshaft.aov$SD <- (abs(csa.midshaft.aov$upr - csa.midshaft.aov$lwr))/2 #This is 2 standard deviations for a 95% confidence interval-- if doing 1 sd away from the mean, just divide this by 1/2
#csa.midshaft.aov$Std.E <- csa.midshaft.aov$SD/(sqrt(length(csa.midshaft.aov$SD)))



#Now the same function, but with the raw data in order to perform the statistical tests (this is just repeating all steps up to, but not including calculating percentages) 

midshaft.raw.csa.function <- function(x)
{
  slicelength <- as.numeric(length(csa[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slice.bind <- cbind(seq(1, slicelength), (csa[which(label == x)]))
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  csa.levels <- data.frame(rbind(slice.matrix[,2]))
  csa.levels <- cbind(x, csa.levels)
  colnames(csa.levels) <- c("specimen", seq(0,95, by=5))
  return(csa.levels)
}


raw.csa.result = mdply(unique(x), midshaft.raw.csa.function)
raw.result.csa.long <- melt(raw.csa.result, variable.name="level", value.name="raw.value")

raw.result.csa.long <- rbind(raw.result.csa.long[which(raw.result.csa.long$level == 30),], 
                             raw.result.csa.long[which(raw.result.csa.long$level == 35),],
                             raw.result.csa.long[which(raw.result.csa.long$level == 40),],
                             raw.result.csa.long[which(raw.result.csa.long$level == 45),],
                             raw.result.csa.long[which(raw.result.csa.long$level == 50),],
                             raw.result.csa.long[which(raw.result.csa.long$level == 55),],
                             raw.result.csa.long[which(raw.result.csa.long$level == 60),],
                             raw.result.csa.long[which(raw.result.csa.long$level == 65),],
                             raw.result.csa.long[which(raw.result.csa.long$level == 70),])

#*****Need to work on this code to make a Repeated measures ANOVA####
#raw.csa.result.diaphysis <- rbind(raw.result.csa.long[which(raw.result.csa.long$level == 30),], 
#                                  raw.result.csa.long[which(raw.result.csa.long$level == 35),],
#                                 raw.result.csa.long[which(raw.result.csa.long$level == 40),],
#                                  raw.result.csa.long[which(raw.result.csa.long$level == 45),],
#                                  raw.result.csa.long[which(raw.result.csa.long$level == 50),],
#                                  raw.result.csa.long[which(raw.result.csa.long$level == 55),],
#                                  raw.result.csa.long[which(raw.result.csa.long$level == 60),],
#                                  raw.result.csa.long[which(raw.result.csa.long$level == 65),],
#                                  raw.result.csa.long[which(raw.result.csa.long$level == 70),])
#m.raw <- aov(raw.csa.result.diaphysis$raw.value ~ raw.csa.result.diaphysis$level)
#csa.m.rawtuk <- TukeyHSD(m.raw)
#write.table(csa.m.tuk[1], file="Human_femur_aov_csa_raw.txt", sep="\t")

#T-test of all levels against midshaft

csa.ttest <-as.data.frame(cbind( c(30,35,40,45,55,60, 65,70), c(t.test(raw.csa.result$'50', raw.csa.result$'30', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'35', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'40', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'45', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'55', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'60', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'65', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'70', paired=T)$p.value)))
colnames(csa.ttest) <- c("level", "p-value")
#Bonferroni Correction to these p-values
csa.ttest[,2] <- p.adjust(csa.ttest[,2], method = "bonferroni", n = length(csa.ttest[,2]))
write.table(csa.ttest, file="Human_Femur_csa_ttest.txt", sep="\t")




####################################################################################################################################################################################
#J analysis####
midshaft.J.function <- function(x)
{
  slicelength <- as.numeric(length(J[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slice.bind <- cbind(seq(1, slicelength), (J[which(label == x)]))
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  J.levels <- data.frame(rbind(slice.matrix[,2]))
  J.levels <- cbind(x, J.levels)
  colnames(J.levels) <- c("specimen", seq(0,95, by=5))
  perdiff <- rbind(((J.levels[,2:21] -J.levels[,12])/J.levels[,12])*100)/length(unique(x))
  perdiff <- data.frame(cbind(x, perdiff))
  colnames(perdiff) <- c("specimen",seq(0,95, by=5))
  return(perdiff)
}


x <- as.vector(data$label)



result.J = mdply(unique(x), midshaft.J.function)
result.J.long <- melt(result.J, variable.name="level", value.name="percent.difference")
result.J.long  <- rbind(result.J.long[which(result.J.long$level == 30),], 
                        result.J.long[which(result.J.long$level == 35),],
                        result.J.long[which(result.J.long$level == 40),],
                        result.J.long[which(result.J.long$level == 45),],
                        result.J.long[which(result.J.long$level == 50),],
                        result.J.long[which(result.J.long$level == 55),],
                        result.J.long[which(result.J.long$level == 60),],
                        result.J.long[which(result.J.long$level == 65),],
                        result.J.long[which(result.J.long$level == 70),])

#*****Need to work on this code to make a Repeated measures ANOVA####
#See CSA for name changes
#m <- aov(J.result.diaphysis$percent.difference ~ J.result.diaphysis$level)
#J.m.tuk <- TukeyHSD(m)
#write.table(J.m.tuk[1], file="Human_femur_aov_J.txt", sep="\t")

#Pull out the mean percent differences from the TukeyHSD at each level of interest
#midshaft.aov <- c('50-30','50-35','50-40','50-45', '55-50', '60-50', '65-50','70-50')
#J.midshaft.aov <- as.data.frame(J.m.tuk$'J.result.diaphysis$level'[midshaft.aov,])
#J.midshaft.aov['50-50',] <- 0
#J.midshaft.aov$level <- c(30, 35, 40, 45, 55, 60, 65, 70, 50)
#J.midshaft.aov <- abs(J.midshaft.aov)
#J.midshaft.aov$SD <- (abs(J.midshaft.aov$upr - J.midshaft.aov$lwr))/2 #This is 2 standard deviations for a 95% confidence interval-- if doing 1 sd away from the mean, just divide this by 1/2
#J.midshaft.aov$Std.E <- J.midshaft.aov$SD/(sqrt(length(J.midshaft.aov$SD)))


#Now the same function, but with the raw data in order to perform the statistical tests (all steps up to, but excluding calculating percentages)

midshaft.raw.J.function <- function(x)
{
  slicelength <- as.numeric(length(J[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slice.bind <- cbind(seq(1, slicelength), (J[which(label == x)]))
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  J.levels <- data.frame(rbind(slice.matrix[,2]))
  J.levels <- cbind(x, J.levels)
  colnames(J.levels) <- c("specimen", seq(0,95, by=5))
  return(J.levels)
}

raw.J.result = mdply(unique(x), midshaft.raw.J.function)
raw.result.J.long <- melt(raw.J.result, variable.name="level", value.name="raw.value")

raw.result.J.long <- rbind(raw.result.J.long[which(raw.result.J.long$level == 30),], 
                           raw.result.J.long[which(raw.result.J.long$level == 35),],
                           raw.result.J.long[which(raw.result.J.long$level == 40),],
                           raw.result.J.long[which(raw.result.J.long$level == 45),],
                           raw.result.J.long[which(raw.result.J.long$level == 50),],
                           raw.result.J.long[which(raw.result.J.long$level == 55),],
                           raw.result.J.long[which(raw.result.J.long$level == 60),],
                           raw.result.J.long[which(raw.result.J.long$level == 65),],
                           raw.result.J.long[which(raw.result.J.long$level == 70),])


#m.raw <- aov(raw.J.result.diaphysis$raw.value ~ raw.J.result.diaphysis$level)
#J.m.rawtuk <- TukeyHSD(m.raw)
#write.table(J.m.tuk[1], file="Human_femur_aov_J_raw.txt", sep="\t")

#T-test of all levels against midshaft
J.ttest <-as.data.frame(cbind( c(30,35,40,45,55,60, 65,70), c(t.test(raw.J.result$'50', raw.J.result$'30', paired=T)$p.value,
                                                              t.test(raw.J.result$'50', raw.J.result$'35', paired=T)$p.value,
                                                              t.test(raw.J.result$'50', raw.J.result$'40', paired=T)$p.value,
                                                              t.test(raw.J.result$'50', raw.J.result$'45', paired=T)$p.value,
                                                              t.test(raw.J.result$'50', raw.J.result$'55', paired=T)$p.value,
                                                              t.test(raw.J.result$'50', raw.J.result$'60', paired=T)$p.value,
                                                              t.test(raw.J.result$'50', raw.J.result$'65', paired=T)$p.value,
                                                              t.test(raw.J.result$'50', raw.J.result$'70', paired=T)$p.value)))

colnames(J.ttest) <- c("level", "p-value")
J.ttest[,2] <- p.adjust(J.ttest[,2], method = "bonferroni", n = length(J.ttest[,2]))
write.table(J.ttest, file="Human_Femur_J_ttest.txt", sep="\t")


####################################################################################################################################################################################
#Imin/Imax ratio analysis####
midshaft.ratio.function <- function(x)
{
  slicelength <- as.numeric(length(ratio[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slice.bind <- cbind(seq(1, slicelength), (ratio[which(label == x)]))
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  ratio.levels <- data.frame(rbind(slice.matrix[,2]))
  ratio.levels <- cbind(x, ratio.levels)
  colnames(ratio.levels) <- c("specimen", seq(0,95, by=5))
  #perdiff <- rbind(((csa.levels[,2:21] - csa.levels[,12])/csa.levels[,12])*100)/length(unique(x))
  perdiff <- data.frame(cbind(x, perdiff))
  colnames(perdiff) <- c("specimen",seq(0,95, by=5))
  return(perdiff)
}


x <- as.vector(data$label)



result.ratio = mdply(unique(x), midshaft.ratio.function)
result.ratio.long <- melt(result.ratio, variable.name="level", value.name="percent.difference")
result.ratio.long <- rbind(result.ratio.long[which(result.ratio.long$level == 30),], 
                           result.ratio.long[which(result.ratio.long$level == 35),],
                           result.ratio.long[which(result.ratio.long$level == 40),],
                           result.ratio.long[which(result.ratio.long$level == 45),],
                           result.ratio.long[which(result.ratio.long$level == 50),],
                           result.ratio.long[which(result.ratio.long$level == 55),],
                           result.ratio.long[which(result.ratio.long$level == 60),],
                           result.ratio.long[which(result.ratio.long$level == 65),],
                           result.ratio.long[which(result.ratio.long$level == 70),])

#*****Need to work on this code to make a Repeated measures ANOVA####
#m <- aov(ratio.result.diaphysis$percent.difference ~ ratio.result.diaphysis$level)
#ratio.m.tuk <- TukeyHSD(m)
#write.table(ratio.m.tuk[1], file="Human_femur_aov_ratio.txt", sep="\t")

#midshaft.aov <- c('50-30','50-35','50-40','50-45', '55-50', '60-50', '65-50','70-50')
#ratio.midshaft.aov <- as.data.frame(ratio.m.tuk$'ratio.result.diaphysis$level'[midshaft.aov,])
#ratio.midshaft.aov['50-50',] <- 0
#ratio.midshaft.aov$level <- c(30, 35, 40, 45, 55, 60, 65, 70, 50)
#ratio.midshaft.aov <- abs(ratio.midshaft.aov)
#ratio.midshaft.aov$SD <- abs(ratio.midshaft.aov$upr - ratio.midshaft.aov$lwr) #This is 2 standard deviations for a 95% confidence interval-- if doing 1 sd away from the mean, just divide this by 1/2
#ratio.midshaft.aov$Std.E <- ratio.midshaft.aov$SD/(sqrt(length(ratio.midshaft.aov$SD)))



#Now the same function, but with the raw data in order to perform the statistical tests (all steps up to, but excluding calculating percentages)
midshaft.raw.ratio.function <- function(x)
{
  slicelength <- as.numeric(length(ratio[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slice.bind <- cbind(seq(1, slicelength), (ratio[which(label == x)]))
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  ratio.levels <- data.frame(rbind(slice.matrix[,2]))
  ratio.levels <- cbind(x, ratio.levels)
  colnames(ratio.levels) <- c("specimen", seq(0,95, by=5))
  return(ratio.levels)
}


raw.ratio.result = mdply(unique(x), midshaft.raw.ratio.function)
raw.result.ratio.long <- melt(raw.ratio.result, variable.name="level", value.name="raw.value")

raw.result.ratio.long <- rbind(raw.result.ratio.long[which(raw.result.ratio.long$level == 30),], 
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 35),],
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 40),],
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 45),],
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 50),],
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 55),],
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 60),],
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 65),],
                               raw.result.ratio.long[which(raw.result.ratio.long$level == 70),])

#m.raw <- aov(raw.ratio.result.diaphysis$raw.value ~ raw.ratio.result.diaphysis$level)
#ratio.m.rawtuk <- TukeyHSD(m.raw)
#write.table(ratio.m.tuk[1], file="Human_femur_aov_ratio_raw.txt", sep="\t")



ratio.ttest <-as.data.frame(cbind( c(30,35,40,45,55,60, 65,70), c(t.test(raw.ratio.result$'50', raw.ratio.result$'30', paired=T)$p.value,
                                                                  t.test(raw.ratio.result$'50', raw.ratio.result$'35', paired=T)$p.value,
                                                                  t.test(raw.ratio.result$'50', raw.ratio.result$'40', paired=T)$p.value,
                                                                  t.test(raw.ratio.result$'50', raw.ratio.result$'45', paired=T)$p.value,
                                                                  t.test(raw.ratio.result$'50', raw.ratio.result$'55', paired=T)$p.value,
                                                                  t.test(raw.ratio.result$'50', raw.ratio.result$'60', paired=T)$p.value,
                                                                  t.test(raw.ratio.result$'50', raw.ratio.result$'65', paired=T)$p.value,
                                                                  t.test(raw.ratio.result$'50', raw.ratio.result$'70', paired=T)$p.value)))

colnames(ratio.ttest) <- c("level", "p-value")

ratio.ttest[,2] <- p.adjust(ratio.ttest[,2], method = "bonferroni", n = length(ratio.ttest[,2]))
write.table(ratio.ttest, file="Human_Femur_ratio_ttest.txt", sep="\t")




####################################################################################################################################################################################
#****Need to edit to reflect Sladek formula: Combined Plot####
pdf("Human Femur Combined.pdf", height=21, width=35)
#Plot the mean differences (absolute) from the Tukey of the percent difference data, along with the standard deviations for those values
result.diaphysis.plot <- ggplot(csa.midshaft.aov, aes(csa.midshaft.aov$level, csa.midshaft.aov$diff*100, group=1)) +
  scale_colour_manual("", breaks=c("CSA","J","Imax/Imin"), values = c("blue", "orange", "dark red"))+
  scale_fill_manual("", breaks=c("CSA","J","Imax/Imin"), values = c("light blue", "yellow", "red"))+
  #CSA Line with Std.E bands
  geom_line(stat='smooth', sHuman=.5, aes(colour="CSA", fill="CSA"), size=2) + 
  geom_ribbon(aes(ymax=(csa.midshaft.aov$diff+ (csa.midshaft.aov$Std.E))*100 , ymin= (csa.midshaft.aov$diff - (csa.midshaft.aov$Std.E))*100, fill="CSA"), alpha=.2)+
  #J Line with Std.E bands
  geom_line(data= J.midshaft.aov,aes(level, diff*100, colour="J", fill="J"), stat='smooth', sHuman=.5, size=2) + 
  geom_ribbon(data= J.midshaft.aov, aes(ymax=(J.midshaft.aov$diff+ (J.midshaft.aov$Std.E))*100 , ymin= (J.midshaft.aov$diff - (J.midshaft.aov$Std.E))*100, fill="J"), alpha=.2)+
  #Imax/Imin Ratio Line with Std.E bands
  geom_line(data= ratio.midshaft.aov,aes(level, diff*100, colour="Imax/Imin", fill="Imax/Imin"), stat='smooth', sHuman=.5, size=2) + 
  geom_ribbon(data= ratio.midshaft.aov, aes(ymax=(ratio.midshaft.aov$diff+ (ratio.midshaft.aov$Std.E))*100 , ymin= (ratio.midshaft.aov$diff - (ratio.midshaft.aov$Std.E))*100, fill="Imax/Imin"), alpha=.3)


#Add Theme
result.diaphysis.plot + xlab('5% Intervals Along Diaphysis')+ ylab('Percent Difference from Midshaft') + theme_calc(base_size=28) + 
  theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust = .5, angle = 90)) 
dev.off()



##########################
#More simple plot that Ian preferred ####
ggplot(result.csa.long, aes(result.csa.long$level, result.csa.long$percent.difference, group=1)) +
  geom_point() 

#probably need to leave trunkated plot called result.csa.long on all files