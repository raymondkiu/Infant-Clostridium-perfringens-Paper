# Grouped by CPA-NEC to compare if NEC pfo+ is greater than non-NEC pfo+
data <- read.csv("cytotoxicity_assay_CPANEC_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Lineage <- factor(data$Lineage, levels = c("I","II","III","IV","V","VI","VII","VIII"))
data$Group2 <- factor(data$Group2, levels = c("CPA-NEC","Non-CPA-NEC"))

p <- 
  ggplot(data,aes(x=Group2, y=Perc, fill=Group2)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0, 10,20,30,40,50,60,70,80), limits=c(-2,100)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+ xlab("") +ylab("")+
  scale_fill_manual(values=c("CPA-NEC"="mediumpurple1","Non-CPA-NEC"="grey"))
p

## Stats for NEC pfo+ vs non-NEC pfo-
head(data)
library(FSA)
Summarize(Perc~ Group2, data = data)
# Any difference in stats -> normality test, if normal use ANOVA if not use Kruskal Wallis.
shapiro.test(data$Perc)

wilcox.test(Perc ~ Group2, data=data, alternative="two.sided")
