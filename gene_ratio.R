de_report = read.table("DE_proteins_report.tsv", header = T, sep = "\t")

library(ggplot2)
library(tidyverse)

de_report$gene_ratio =  de_report$Significant / de_report$Annotated

de_gg = de_report %>%
  ggplot(aes(x = gene_ratio, 
             y = Term, 
             color = pvalues,
             size = Significant)) +
  geom_point(stat = "identity") +
  scale_color_continuous(low = "red", high = "yellow") +
  labs(color = "P-value", size = "Count", x = "Gene Ratio (Significant:Annotated)", y = "Gene Ontology Term") +
  theme_bw()

de_gg

