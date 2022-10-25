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

# To save files in csv:
write.csv(hb.results$partition.df, file = "hierbaps_partition-snp4-unique-dontkeepsingletons-6.csv")

# Alternative to hierbaps -> rPineCone maybe a better one with respect to tracking SNPs and epidemilogy
# run rpinecone = distance based method
devtools::install_github('alexwailan/rpinecone')
# devtools::install_github('alexwailan/rpinecone')
library(rPinecone)
library(ape)
library(phytools)
library(adegenet) #fasta2DNAbin

# Set working directory (path to your file)
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure1/")

# Prepare files: pyjar (newick tree + fasta SNP alignment) .tre is the output from pyjar.py
tree <- ape::read.tree("snp4treeforpinecone.tree.joint.tre")

# Run pinecone
results <- pinecone(tree, 20, 10)

itol_sublineage_output(results)
itol_major_SL_output(results)
