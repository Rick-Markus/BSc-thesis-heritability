library(heritability)
library(tidyverse)

results <- read.table("C:/Users/Gebruiker/Wageningen University & Research/Lisa van Sluijs - Rick Markus/Research/External Data/heritability data geordend.csv", header = T, sep = ";")
results <- results[, 1:8]
results <- na.omit(results)
results[, "strain"] <- as.factor(results[,"strain"])
results[,"ethanol"] <- as.factor(results[, "ethanol"])
results[, "replicate"] <- as.factor(results[, "replicate"])
results[, "super_replicate"] <- as.factor(results[, "super_replicate"])
results[, "strain_no"] <- as.factor(results[, "strain_no"])

data_nor_broodsize <- function(data) {
  for (strain in unique(data$strain)) {
    mean <- sum(data$broodsize[data$strain == strain & data$ethanol == 0]) / length(data$broodsize[data$strain == strain & data$ethanol == 0])
    data$broodsize_nor[data$strain == strain] <- data$broodsize[data$strain == strain] / mean   
  }
  return(data)
}

#1 for data Rick; 2 for data Marijke #remove filter for all data
#results <- results[results$super_replicate == 2,]

results <- data_nor_broodsize(results)

H2_data <- function(results) {  
  repeatability <- list()
  for (eth_con in unique(results$ethanol)) { # unique zorgt ervoor dat je de unieke eth_con gebruikt (e.g. factor levels ipv eerst 48 keer 0 etc.)
    data <- repeatability(data.vector = results$broodsize_nor[results$ethanol == eth_con], geno.vector = results$strain[results$ethanol == eth_con])
    H2 <- data$repeatability
  #key_name <- paste("the repeatability (H2) for", eth_con, "mM is:")
    repeatability[[as.character(eth_con)]] <-  H2}
  return(repeatability)  
}

H2_sig <- function(results){
  number_of_permutations = rep(0, times = 1000)
  alpha <- 0.05
  percentile <- 1-alpha
  sig_H2 <- list()

  for (eth_con in unique(results$ethanol)) { 
    repeatability_perm <- rep(0, times = length(number_of_permutations))
    for (j in seq_along(number_of_permutations)) {
      results$broodsize_nor <- sample(results$broodsize_nor) 
      data <- repeatability(data.vector = results$broodsize_nor[results$ethanol == eth_con], geno.vector = results$strain[results$ethanol == eth_con])
      H2 <- data$repeatability
      repeatability_perm[j] <- H2}
    repeatability_perm_sorted <- sort(repeatability_perm)
    significance_H2 <- repeatability_perm_sorted[percentile*length(number_of_permutations)]
    #key_name <- paste("the repeatability (H2) significance level for", eth_con, "mM is:") 
    sig_H2[[as.character(eth_con)]] <- significance_H2
    }
  return(sig_H2)
}


H2_plot <- function(results){
  repeatability_eth <- data.frame(
    ethanol = unique(results$ethanol), 
    repeatability = unname(unlist(H2_data(results))),
    sig_repeatability = unname(unlist(H2_sig(results))))
  print(repeatability_eth)
  repeatability_eth[2:5,] %>%
    ggplot(aes(ethanol, repeatability))+
      geom_col()+ 
      geom_errorbar(aes(ymin = sig_repeatability, ymax = sig_repeatability), size = 1.75, color = "red", alpha = 0.6)+ 
      labs( x = "Ethanol concentration (mM)", y = "H2")
}

H2_data(results)
H2_sig(results)
H2_plot(results)

