#strains Marijke: ECA396, JU1400, MY2147, and NIC526 #data Rick
#strains mij hoger: (ECA396), JU2600 #data Rick 
#voor elke eth_con strain ECA396 en ECA36 vergelijken # dit doen met gecombineerde data (QTL mappen) # dit in supplemental info zetten

library(tidyverse)
library(forcats)
library(dplyr)
library(ggsignif)

results = read.csv2("C:/Users/Gebruiker/Wageningen University & Research/Lisa van Sluijs - Rick Markus/Research/Analysis/Data analysis results Marijke/CSV files analysis/Ethanol-Broodsize data.csv", header = T)
results[,4] <- as.factor(results[,4])
results[,5] <- as.factor(results[,5])
results[,1] <- as.factor(results[,1])
results[,2] <- as.factor(results[,2])
results[,3] <- as.factor(results[,3])

results = na.omit(results)

data_nor_broodsize <- function(data) {
  for (strain in unique(data$strain)) {
    mean <- sum(data$broodsize[data$strain == strain & data$ethanol == 0]) / length(data$broodsize[data$strain == strain & data$ethanol == 0])
    data$broodsize_nor[data$strain == strain] <- data$broodsize[data$strain == strain] / mean   
  }
  return(data)
}

#1 for data Rick; 2 for data Marijke #remove filter for all data
results <- results[results$super_replicate == 1,]

results <- data_nor_broodsize(results)
mean(results$broodsize_nor[results$strain == "DL238" & results$ethanol == 0])

results_0_100 <- results[results$ethanol == 0 | results$ethanol == 100,]
sub_results <- results_0_100[results_0_100$strain == "ECA396" | results_0_100$strain == "JU1400" | results_0_100$strain == "MY2147" | results_0_100$strain == "NIC526" | results_0_100$strain == "JU2600",]

#to add signficance
annotation_data <- data.frame(
  strain = c("ECA396"),   
  x = c(1.5),                  
  y = c(1.75),                    
  label = c("*"))

#to place JU2600 on the right
sub_results$strain <- factor(sub_results$strain, levels = c("CB4856", "DL238", "ECA36", "ECA396", "EG4725", "JU1400", "JU2526", 
                                                            "JU310", "MY2147", "MY2693", "N2", "NIC2", "NIC526", 
                                                            "QX1794", "XZ1516", "JU2600"))

sub_results %>%
    ggplot(aes(ethanol, broodsize_nor))+
    geom_boxplot()+
    facet_wrap(~strain, nrow = 1)+
    labs(x = "Ethanol concentration (mM)", y = "Normalized brood size")+
    geom_text(
    data = annotation_data,
    aes(x = x, y = y, label = label),
    size = 7,
    color = "red",
    inherit.aes = FALSE)

