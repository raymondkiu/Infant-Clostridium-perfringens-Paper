
data <- read.csv("cytotoxicity_assay_data_for_stats.csv")
head(data)
library(FSA)
library(rstatix)
options(dplyr.print_max = 1e9) # print all possible tibble rows, for dunn_test mainly
Summarize(Perc~ Lineage, data = data)
# Any difference in stats -> normality test, if normal use ANOVA if not use Kruskal Wallis?
shapiro.test(data$Perc)

res.kruskal <- kruskal.test(Perc ~ Lineage, data=data)
res.kruskal

## Test which lineage is statistically different, suitable for unequal number of observations
dunn_test(data, Perc ~ Lineage, p.adjust.method = "BH") # better package to visualise stats

# See which strain is different from untreated
res.kruskal <- kruskal.test(Perc ~ Strain, data=data)
res.kruskal

dunn_test(data, Perc ~ Strain, p.adjust.method = "BH")

# use this options(max.print=1000000) to print more rows (unlimited)

# To test for toxin gene correlatino with increase of cell death percentage:
# point-biserial correlation to correlate cell death percentage with toxin gene profile
# PfoA gene only:
Data <- read.csv("cytotoxicity_assay_point_biserial_stats.csv", header = TRUE, stringsAsFactors = TRUE)
Data

# Input format (binary for toxin gene presence)
#Strain plc pfoA CellDeathMean
#IQ146 1 1 50

# Store into variable, x is binomial, y is continuous variable
x <- Data$pfoA # do this also for cpb2 toxin gene, Data$cpb2
x
y <- Data$Perc
y

# use Spearman for ranking if data is not normal.
cor.test(x, y,method="spearman",alternative="two.sided",exact=FALSE)

# Explanation:, correlation coefficient is 0.85, positive means that if x is 1, y tends to be higher. and P value
# is 0.001, which is <0.05, significant, so the correlation is significant.
