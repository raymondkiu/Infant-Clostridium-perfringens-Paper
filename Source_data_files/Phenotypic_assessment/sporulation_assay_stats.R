# stats: use Kruskal-Wallis, non-parametric on which isolate is significantly different
Data <- read.csv("sporulation_assay_for_stats.csv", header = TRUE, stringsAsFactors = TRUE)
Data

library(FSA)
library(rstatix)

Summarize(Spores~ Lineage, data = Data)
shapiro.test(Data$Spores) # test for data normality

res.kruskal <- kruskal.test(Spores ~ Lineage, data=Data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations
dunn_test(Data, Spores ~ Lineage, p.adjust.method = "BH")
