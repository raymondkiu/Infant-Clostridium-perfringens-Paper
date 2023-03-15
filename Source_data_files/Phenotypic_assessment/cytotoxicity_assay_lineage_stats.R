data <- read.csv("cytotoxicity_assay_lineage_data_for_plotting.csv")
head(data)
library(FSA)
library(rstatix)

options(dplyr.print_max = 1e9) # print all possible tibble rows, for dunn_test mainly
Summarize(Perc~ Lineage, data = data)
# Any difference in stats -> normality test, if normal use ANOVA if not use Kruskal Wallis
shapiro.test(data$Perc)

res.kruskal <- kruskal.test(Perc ~ Lineage, data=data)
res.kruskal

## Test which lineage is statistically different, suitable for unequal number of observations
dunn_test(data, Perc ~ Lineage, p.adjust.method = "BH") # better package to visualise stats
