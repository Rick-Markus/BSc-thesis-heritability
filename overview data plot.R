library(tidyverse)

results <- read.csv2("C:/Users/Gebruiker/Wageningen University & Research/Lisa van Sluijs - Rick Markus/Research/Analysis/Data analysis results Marijke/CSV files analysis/Ethanol-Broodsize data.csv", header = T)

results[,4] <- as.factor(results[,4])
results[,5] <- as.factor(results[,5])
results[,1] <- as.factor(results[,1])
results[,2] <- as.factor(results[,2])
results[,3] <- as.factor(results[,3])

results <- na.omit(results)

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

attach(results)
means <- aggregate(broodsize_nor ~ strain + ethanol, results , mean)

se <- function(x) {
  sd(x)/sqrt(length(x))
}

standard_error = aggregate(broodsize_nor ~ strain + ethanol, results, FUN = se)
standard_error = standard_error$broodsize_nor  
means <- cbind(means, standard_error)
colnames(means) <- c("Strain", "ethanol", "broodsize_nor", "standard_error")

means$ethanol_good[means$ethanol == 0] <- 0
means$ethanol_good[means$ethanol == 100] <- 100
means$ethanol_good[means$ethanol == 200] <- 200
means$ethanol_good[means$ethanol == 300] <- 300
means$ethanol_good[means$ethanol == 400] <- 400

means <- means %>%
  mutate(
    jittered_ethanol = ethanol_good + runif(n(), min = -5, max = 5)  
  )

custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
                   "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#9edae5", "#c5b0d5", 
                   "#f7b6d2", "#c49c94", "#ffbb78", "#98df8a") 

means %>%
  ggplot(aes(jittered_ethanol, broodsize_nor, colour = Strain, group = Strain)) +
  geom_point(aes(colour = Strain), size = 3, alpha = 0.4) +
  geom_line(alpha = 0.2, size = 1.75) +
  geom_errorbar(
    aes(x = jittered_ethanol,
        ymin = broodsize_nor - standard_error, 
        ymax = broodsize_nor + standard_error),
    width = 0.01, 
    alpha = 0.4) +
  scale_color_manual(values = custom_colors)+
  labs(x = "Ethanol concentration (mM)", 
    y = "Normalized brood size")+
  theme_minimal()


