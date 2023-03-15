# final boxplot requested by editor:
data <- read.csv("cytotoxicity_assay_lineage_data_for_plotting.csv", header = TRUE, stringsAsFactors = TRUE) # reload data
head(data)
data$Lineage <- factor(data$Lineage, levels = c("Untreated", "I","III","V","VI","VII"))

p <- 
  ggplot(data,aes(x=Lineage, y=Perc, fill=Lineage)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80), limits=c(-2,80)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+ xlab("") +ylab("")+
  scale_fill_manual(values=c("I"="#ba1114","III"="#ebda26","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71"))
p
