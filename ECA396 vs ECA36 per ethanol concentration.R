library(tidyverse)

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
#results <- results[results$super_replicate == 1,]

results <- data_nor_broodsize(results)
mean(results$broodsize_nor[results$strain == "DL238" & results$ethanol == 0])

sub_results <- results[results$strain == "ECA396" | results$strain == "ECA36",]
sub_results <- sub_results[sub_results$ethanol != 0,]

annotation_data <- data.frame(
  ethanol = c("100", "200", "300", "400"),   
  x = c(1.5, 1.5, 1.5, 1.5),                  
  y = c(3.5, 3.5, 2, 2),                    
  label = c("***", "***", "***", "**"))
#* for p = 0.05; ** for 0.01 *** for 0.001

sub_results %>%
  ggplot(aes(strain, broodsize_nor))+
  geom_boxplot()+
  facet_wrap(~ethanol, labeller = labeller(ethanol = c("100" = "100 mM", "200"  = "200 mM", "300" = "300 mM", "400" = "400 mM")), nrow = 1)+
  labs(x = "Strain", y = "Normalized brood size")+
  geom_text(
    data = annotation_data,
    aes(x = x, y = y, label = label),
    size = 7,
    color = "red",
    inherit.aes = FALSE)+
  scale_y_continuous(limits = c(NA, 4))

  

