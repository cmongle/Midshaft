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

#or as absolute percent difference

ggplot(result.csa.long, aes(result.csa.long$level, abs(result.csa.long$percent.difference), group=1)) +
  geom_point() +geom_line(stat='smooth')




#try to apply this function to calculate Standard error at each level since I am no longer using the anova around the percent difference
library(plotrix)
#needs to be mean percent difference + this value and then -this value
std.error(result.csa$'30')
