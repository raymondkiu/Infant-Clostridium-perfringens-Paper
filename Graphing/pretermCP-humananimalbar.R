library(tidyverse)
library(ggpubr)


# Make a pie chart based on human animal isolate percentages
##############################################
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure1/Pie-chart-human-animals/")
data <- read.csv("Piecharts.csv")
head(data)

# melt data
plotDat <- reshape::melt(data)
plotDat <- as.data.frame(plotDat)
print(plotDat)
# filter data
piechart1<- filter (plotDat, Lineage == "VIII")
#head(piechart1)
# plot bar chart first then convert - this is the usual way
HumanAnimalPalette <- c("Humans"="#535756",
                        "Animals"="#AEBBCF",
                        "Unknown"="Grey96"
)


pc1<- ggplot(plotDat, aes(x="", y=value, fill=variable))+
  scale_fill_manual(values=HumanAnimalPalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank())+
  ylab("") + xlab("")

pc1

# plot bar charts
# Reorder bar
plotDat$variable <- factor(plotDat$variable, levels = c("Unknown", "Animals","Humans"))

bc1 <- ggplot(plotDat, aes(x=Lineage, y=value, fill=variable),position_stack(reverse = TRUE))+
  scale_fill_manual(values =HumanAnimalPalette)+
  geom_bar(stat = "identity")+
  ylab("") + xlab("")+
  scale_y_continuous(breaks=c(0,25,50,75,100), limits=c(0,100)) + theme(axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank())
bc1
