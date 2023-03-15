# Grouped by PFoA+cpb2 to compare if pfo+cpb2 has a synergistic effect 
library(Rmisc)
library('cowplot')
theme_set(theme_cowplot())
data <- read.csv("cytotoxicity_assay_pfoA_cpb2_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Lineage <- factor(data$Lineage, levels = c("I","II","III","IV","V","VI","VII","VIII"))
data$Group <- factor(data$Group, levels = c("pfoA+","pfoA+cpb2+"))
#data <- summarySE(data, measurevar="Perc", groupvars=c("Group")) # Group first then Isolate
#data

p <- 
  ggplot(data,aes(x=Group, y=Perc, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80), limits=c(-2,100)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1,face = "italic"))+ xlab("") +ylab("")+
  scale_fill_manual(values=c("pfoA+cpb2+"="pink","pfoA+"="red2"))

p

# stats for pfoA+cpb2+ = 2 groups so use Wilcox (t-test for parametric test, two-sided by default)
wilcox.test( Perc ~ Group, data = data)
