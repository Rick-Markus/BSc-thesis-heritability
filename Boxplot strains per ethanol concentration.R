library(tidyverse)
library(dplyr)
library(tidytext)

results <- read.csv2("C:/Users/Gebruiker/Wageningen University & Research/Lisa van Sluijs - Rick Markus/Research/Analysis/Data analysis results Marijke/CSV files analysis/Ethanol-Broodsize data.csv", header = T)

results[,4] <- as.factor(results[,4])
results[,5] <- as.factor(results[,5])
results[,1] <- as.factor(results[,1])
results[,2] <- as.factor(results[,2])
results[,3] <- as.factor(results[,3])

data_nor_broodsize <- function(data) {
  for (strain in unique(data$strain)) {
    mean <- sum(data$broodsize[data$strain == strain & data$ethanol == 0]) / length(data$broodsize[data$strain == strain & data$ethanol == 0])
    data$broodsize_nor[data$strain == strain] <- data$broodsize[data$strain == strain] / mean   
  }
  return(data)
}

results <- na.omit(results)

#1 for data Rick; 2 for data Marijke #remove filter for all data
#results <- results[results$super_replicate == 1,]

results <- data_nor_broodsize(results)

results %>%
  ggplot(aes(reorder_within(strain, broodsize_nor, ethanol, median), broodsize_nor))+
  geom_boxplot()+
  scale_x_reordered()+
  labs(x = "Strain", y = "Normalized brood size")+
  facet_wrap(~ethanol, labeller = labeller(ethanol = c("0" = "0 mM", "100" = "100 mM", "200"  = "200 mM", "300" = "300 mM", "400" = "400 mM")), scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90))