library(rhierbaps)

# Set seed
#set.seed(1234) # To enable reproduction of the same result

# Set working directory (path to your file)
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure1/")


# Load FASTA file  
snp.matrix <- load_fasta("core_gene_snp4-unique.aln", keep.singletons = FALSE)

# Predict clustering (main run) default setting is the best
hb.results <- hierBAPS(snp.matrix, max.depth = 2, quiet =F,n.pops=20,n.cores = 2,n.extra.rounds=Inf)

# Look into your data, first column should be your isolate names
#head(hb.results$partition.df)
#tail(hb.results$partition.df)


