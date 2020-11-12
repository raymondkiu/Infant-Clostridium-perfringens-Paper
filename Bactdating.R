# Install libraries
library(devtools)
library(pkgconfig)
devtools::install_github("xavierdidelot/BactDating")
devtools::install_github("r-lib/pkgbuild", force = TRUE)
library(dplyr)
library(BactDating)
devtools::install_github('xavierdidelot/TransPhylo')
library(TransPhylo)
library(ape)
library(coda)

# To set working directory
setwd("~/Desktop/")

# set seed
set.seed(1234)

# Load csv as object R (column 1 = samples name, column 2 = decimal year)
e <-readLines("name.csv")
# Sample IDs in order no header
d <-as.numeric(readLines("year.csv"))  # coerce character data as numeric data
# Year (decimal) corresponding to name.csv in order no header
# Load into names functions
names(d) <- d
names(d) <- e

# input for ClonalframeML (v1.12)
t=loadCFML(prefix ="clonalframeML-LVI2.out")

# Input tree:
t=read.tree('L4.tree')

sum(t$edge.length) # if below 1, rescale:
#t$edge.length=t$edge.length*sites
#t$edge.length # (ideally > 10)

# To see the tip labels in the tree
t$tip.label

# Rooting tree
rooted=initRoot(t,d,mtry = 50, useRec = T)
rooted
plot(rooted,show.tip.label = T)

# Root to tip analysis: temporal signal
roottotip(rooted, d, rate = NA, permTest = 1000000, showFig = T,showPredInt='gamma',
          colored = T,showText = T, showTree = T)

# Main analysis
res=bactdate(rooted,d,nbIts=1000000,model = "mixedgamma",useRec = T,minbralen = 10, updateRoot=T,
             showProgress = T,useCoalPrior = T,thin = ceiling(10))
res2=bactdate(rooted,d,nbIts=100000,model = "mixedgamma",useRec = T,minbralen = 2, updateRoot=T,
              showProgress = T,useCoalPrior = T,thin = ceiling(100))
#compare two models
modelcompare(res,res2)

plot(res,'treeCI',show.tip.label = T, show.axis = T,cex = .01,)

# Check mcmc >100 each parameter
plot(res,'trace')
mcmc=as.mcmc(res)
effectiveSize(mcmc)

mcmc2=as.mcmc(res2)
effectiveSize(mcmc2)

leafDates(rooted)
nodeDates(rooted)

#######################
## To read the stats ##
#######################
# how to export to figtree to see data clearly (MRCA) - install treeio first
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager") 
BiocManager::install("treeio")
library("treeio")

## Export data to nexus file (contains time scale)
l=as.treedata.resBactDating(res)
obj=methods::new('treedata',phylo=l[[1]],data=dplyr::tbl_df(as.data.frame(l[[2]])))
write.beast(obj,'res.nex')

# Now import into Figtree (https://github.com/rambaut/figtree/releases) v1.4.4
# Branch labels > display 'label' -> get to know which node is which
# Time scale off set -> the latest time in the sample. Use reverse axis to corrrect for the axis orientation.

# Now extract CI for each node:
## Combining tree tip and node labels from res object:
combined <- c(res$tree$tip.label,res$tree$node.label)
## Combining previous object (combined) with CIs:
combinedCIs <- cbind(combined,res$CI)
print(combinedCIs)
