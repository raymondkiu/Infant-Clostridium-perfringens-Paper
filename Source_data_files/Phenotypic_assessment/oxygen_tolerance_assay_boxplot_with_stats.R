# Figure for groups only in boxplots:
Data <- read.csv("oxygen_tolerance_assay_data_stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)

# Reorder groups
Data$Group <- factor(Data$Group, levels = c("PFO+","PFO-"))
Data$Lineage <- factor(Data$Lineage, levels=c("Lineage 1","Lineage 3","Lineage 5","Lineage 6","Lineage 7"))

# Set Time as factor so it will treat numeric data as string - this will work:
Data$Time <- factor(Data$Time, levels=c("0","48","96","144","192","240","288","336"))
p <- 
  ggplot(Data,aes(x=Time, y=Value)) +
  geom_boxplot(aes(fill=Lineage),outlier.colour = "black", outlier.shape = NA, outlier.alpha = 0.2, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_log10(limits = c(1e-8,150),breaks =c(100,10,1,0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001),
                label = c("100", "10","1", "0.1","0.01","0.001","0.0001","0.00001","0.000001","0"))
p+scale_fill_manual(values=c("Lineage 1"="#ba1114","Lineage 3"="#ebda26","Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# For stats on 336h only use Kruskal Wallis, non-parametric alternative of One-way ANOVA:
library(FSA)
Summarize(Value~ Time, data = Data)

Data <- read.csv("oxygen_tolerance_assay_data_stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)

# test if the data is normally distributed, run Kruskal-Wallis as not normal
shapiro.test(Data$Value)

h48 <- filter(Data, Time== "48")
h96 <- filter(Data, Time== "96")
h144 <- filter(Data, Time== "144")
h192 <- filter(Data, Time== "192")
h240 <- filter(Data, Time== "240")
h288 <- filter(Data, Time== "288")
h336 <- filter(Data, Time== "336")

# run Kurskal Wallis as data is normally distributed
kruskal.test(Value ~ Lineage, data=h48)
kruskal.test(Value ~ Lineage, data=h96)
kruskal.test(Value ~ Lineage, data=h144)
kruskal.test(Value ~ Lineage, data=h192)
kruskal.test(Value ~ Lineage, data=h240)
kruskal.test(Value ~ Lineage, data=h288)
kruskal.test(Value ~ Lineage, data=h336)

# mutliple comparison for those groups tested significant
library(rstatix)
dt48 <-dunn_test(Value ~ Lineage, data=h48,p.adjust.method="holm")
dt96 <-dunn_test(Value ~ Lineage, data=h96,p.adjust.method="holm")
dt144 <-dunn_test(Value ~ Lineage, data=h144,p.adjust.method="holm")
dt192 <-dunn_test(Value ~ Lineage, data=h192,p.adjust.method="holm")

print(dt48, n=40)
print(dt96, n=40)
print(dt144, n=40)
print(dt192, n=40)
