#check working directory
getwd()

#load pkg
install.packages("ggsignif")
install.packages("emmeans")
install.packages("phia")
install.packages("ARTool")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("lmerTest")
install.packages("afex")
install.packages("plotly")
install.packages("ggtext")

library(readxl)
library(readr)
library(dplyr)      #data_frame, %>%, filter, summarise, group_by
library(emmeans)    #package to run follow-up comparisons
library(phia)       #testInteractions
library(tidyr)      #spread
library(ARTool)     #art, artlm
library(ggplot2)    #ggplot, stat_…, geom_…, etc
library(ggsignif)
library(tidyverse)  # tidyverse for data manipulation and visualization
library(ggpubr)     # ggpubr for creating easily publication ready plots
library(rstatix)    # rstatix provides pipe-friendly R functions for easy statistical analyses
library(lmerTest)
library(afex)       #package to run ANOVAs
library(plotly)     #creating subplot
library(ggtext)

#set directory
setwd("~/Documents/R")

#import xls file
renal_excel<-read_excel("bms2031 renal xls.xlsx")

#view file
view(renal_excel)

#drinks order
renal_excel$drinks <- factor(renal_excel$drinks, levels = c("Control", "Water", "Coffee", "Powerade"))
class(renal_excel$drinks)
levels(renal_excel$drinks)

#make boxplot
renal_plot <- ggplot(renal_excel, aes(drinks, mean)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) + 
  geom_boxplot(aes(fill = drinks), 
               color = "black", 
               outlier.color = "black") + 
  labs(title = "Mean Urine Flow Rate (ml/min) by Drink Type",
       x = "Drink Type",
       y = "Mean Urine Flow Rate (ml/min)", 
       fill = "Drinks") +
  scale_y_continuous(limits = c(0,NA)) +
  theme_classic2() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Add significance brackets one by one with different y positions
renal_plot <- renal_plot +
  # Control vs Water
  geom_signif(
    comparisons = list(c("Control", "Water")),
    annotations = "****",
    y_position = 7.0,
    tip_length = 0.02
  ) +
  # Control vs Coffee
  geom_signif(
    comparisons = list(c("Control", "Coffee")),
    annotations = "****",
    y_position = 8,
    tip_length = 0.02
  ) +
  # Control vs Powerade
  geom_signif(
    comparisons = list(c("Control", "Powerade")),
    annotations = "****",
    y_position = 9,
    tip_length = 0.02
  ) 

print(renal_plot)

