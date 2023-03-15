### Figure for groups only: boxplot (final)
# Grouped by PFO
data <- read.csv("cytotoxicity_assay_data_for_stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

data$Group <- factor(data$Group, levels = c("Untreated", "pfoA+","pfoA-"))

p <- 
  ggplot(data,aes(x=Group, y=Perc, fill=Group))+
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0, 10,20,30,40,50,60,70,80), limits=c(-1,80)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+ xlab ("") + ylab("")+
  scale_fill_manual(values=c("Control"="lightgreen","pfoA+"="Red2","pfoA-"="Grey"))
p
