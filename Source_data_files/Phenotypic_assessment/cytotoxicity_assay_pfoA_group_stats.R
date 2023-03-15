# to do stats in between lineages:
data <- read.csv("cytotoxicity_assay_data_for_stats")
head(data)
library(FSA)
library(rstatix)
options(dplyr.print_max = 1e9) # print all possible tibble rows, for dunn_test mainly

shapiro.test(data$Perc)

## See differences in between PFO groups?
res.kruskal <- kruskal.test(Perc ~ Group, data=data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations
dunnTest(data$Perc, data$Group,method="bh")
dunn_test(data, Perc ~ Group, p.adjust.method = "BH")
Summarize(Perc~ Group, data = data)
