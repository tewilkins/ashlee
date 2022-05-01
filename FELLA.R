install.packages("BiocManager")
library(BiocManager)

BiocManager::install("FELLA")

library(FELLA)

graph1 = buildGraphFromKEGGREST()

