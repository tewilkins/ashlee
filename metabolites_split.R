metabolites_complete = read.csv("metabolites_complete.csv", sep=',', header = F)
metabolites_dep = read.csv("DEP_metabolites.csv", sep=',', header = T)

library(dplyr)
library(tidyr)

metabolites_split = metabolites_complete %>%
  separate(V1, c("Name","HMDB", "KEGG"), sep = "_")

metabolites_dep_chr = as.data.frame(metabolites_dep$X)
colnames(metabolites_dep_chr) = "Name"
metabolites_dep_complete = merge(metabolites_dep_chr, 
                                 metabolites_split,
                                 by = "Name",
                                 all.x = TRUE)

write.csv(metabolites_split, file = 'metabolites_complete_split.csv', row.names = FALSE)
write.csv(metabolites_dep_complete, file = 'metabolites_DEP_split.csv', row.names = FALSE)


