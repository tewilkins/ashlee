meta_proteomes = read.csv("meta_proteomes.csv", sep = ",", header = T)

install.packages("cluster")

library(dplyr)# ensuring reproducibility for sampling
library(tidyverse)
library(cluster)

spnames = names(meta_proteomes)

all_proteins = meta_proteomes %>%
  pivot_longer(1:8, names_to = "Species", values_to = "Protein") %>%
  filter(Protein != "")

unique_proteins = unique(all_proteins$Protein)
pa_df = data.frame(unique_proteins)
colnames(pa_df) = "Protein"

for (i in 1:length(spnames)){  # iterate over all species
  newcol <- paste0(spnames[i])  # defines a new column with species[i] name
  pa_df[newcol] <- 0  # populates new column with species[i] name full of 0s
  species.iter = all_proteins[all_proteins$Species == paste0(newcol), ]  # creates a subset of biota dataframe where species[i] data exists
  for (j in 1:nrow(pa_df)){
    pa_df[j,newcol] <- ifelse(pa_df$Protein[j] %in% species.iter$Protein, 1, 0)  # sets row to 1 if that species was observed for that sample
  }
}

pa_clust = daisy(pa_df[ , 2:9], metric = c("gower"))
divisive.clust <- diana(as.matrix(pa_clust), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

