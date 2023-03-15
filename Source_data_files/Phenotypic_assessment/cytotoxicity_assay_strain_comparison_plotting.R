 # Strain-level comparison - to be joined by a heatmap of toxin genes (boxplot)
data <- read.csv("cytotoxicity_assay_data_for_stats", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Group <- factor(data$Group, levels = c("pfoA+", "pfoA-","Untreated"))

p <- 
  ggplot(data,aes(x=Strain, y=Perc, fill=Lineage)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80), limits=c(-1,80)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),axis.ticks.x = element_blank())+ xlab("") +ylab("")+
  facet_grid( ~ Group, scales="free")+
  #scale_fill_manual(values=c("Control"="lightgreen","PFO+"="Red2","PFO-"="Grey"))
  scale_fill_manual(values=c("I"="#ba1114","II"="#f79ea4","III"="#ebda26","IV"="#701501","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71","VIII"="#daaff3"))
p

## toxin profile heatmap using ggplot2
setwd("~")
data <- read.csv("cytotoxicity_assay_toxin_profile")  # change to PFO+ or PFO- to access two different csv data files
head(data)
# Melt table first
plotDat <- reshape::melt(data, id=c("Strain","Group"))
head(plotDat)

# apply theme
library('cowplot')
theme_set(theme_cowplot())

## use geom_tile to build heatmap, better than heatmap2, but cannot apply clustering method:
#data$Assay <- factor(data$Assay, levels=c("Necrosis","Apoptosis","H2S","Gas","Haemolysis","Sporulation","Aerotolerance","Generation","Bile","AMR"))
p<-ggplot(data=plotDat) +
  scale_fill_gradient2(low = "white",high="#e5484d")+
  geom_tile(aes(x=Strain, y=variable,fill=value,group=Group),color="lightgrey",size=0.5) + theme_void() +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),legend.position = 'left' )
p
