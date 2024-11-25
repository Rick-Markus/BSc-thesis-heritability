library(heritability)
library(tidyverse)
library(vcfR)
library(dplyr)
library(popkin)

#create genotype matrix
vcf_file <- "C:\\Users\\Gebruiker\\Downloads\\selected_strains.vcf"
vcf <- read.vcfR(vcf_file)

genotype_data <- extract.gt(vcf, element = "GT")

convert_genotype <- function(gt) {
  case_when(
    gt == "0|0" ~ 0,
    gt == "0|1" ~ 1,
    gt == "1|0" ~ 1,
    gt == "1|1" ~ 2,
    TRUE ~ NA_real_  
  )
}
genotype_data <- as.data.frame(genotype_data)

geno_m <- genotype_data %>%
  mutate(across(everything(), convert_genotype))

#calculate kinship matrix
geno_m <- as.matrix(geno_m)

relatedness_m <- popkin(geno_m)

#calculate narrow-sense heritability
data <- read.table("C:/Users/Gebruiker/Wageningen University & Research/Lisa van Sluijs - Rick Markus/Research/External Data/heritability data geordend.csv", header = T, sep = ";")
data <- data[, 1:8]
data <- na.omit(data)

data[,4] <- as.factor(data[,4])
data[,5] <- as.factor(data[,5])
data[,1] <- as.factor(data[,1])
data[,2] <- as.factor(data[,2])
data[,3] <- as.factor(data[,3])

#add broodsize nor values
data_nor_broodsize <- function(data) {
  for (strain in unique(data$strain)) {
    mean <- sum(data$broodsize[data$strain == strain & data$ethanol == 0]) / length(data$broodsize[data$strain == strain & data$ethanol == 0])
    data$broodsize_nor[data$strain == strain] <- data$broodsize[data$strain == strain] / mean   
  }
  return(data)
}

#1 for data Rick; 2 for data Marijke #remove filter for all data
#data <- data[data$super_replicate == 2,]

data <- data_nor_broodsize(data)

h2_data <- function(data, geno_m, relatedness_m) {
  h2 <- c()
  i = 1

  for (eth_con in unique(data$ethanol)){
      h2_temp <- marker_h2(data.vector = data$broodsize_nor[data$ethanol == eth_con], geno.vector = data$strain[data$ethanol == eth_con], K = relatedness_m)
      h2[i] <- h2_temp$h2 
      i <- i + 1  
  }
  return(h2)
}

h2_sig <- function(data, geno_m, number_of_permutations, alpha, relatedness_m) {
  permutations <- rep(0, times = number_of_permutations)
  sig_h2 <- c()
  j <- 1 

  for (eth_con in unique(data$ethanol)){
    temp_h2 <- c()
    for (k in seq_len(number_of_permutations)){
      data$broodsize_nor <- sample(data$broodsize_nor)
      h2 <- marker_h2(data.vector = data$broodsize_nor[data$ethanol == eth_con], geno.vector = data$strain[data$ethanol == eth_con], K = relatedness_m)
      temp_h2[k] <- h2$h2  
    }
    sorted_h2 <- sort(temp_h2)
    sig_h2[j] <- sorted_h2[(1-alpha)*number_of_permutations]
    j <- j + 1
  }
  return(sig_h2)
}

h2_plot <- function(data, geno_m, ethanol, number_of_permutations, alpha, relatedness_m) {
  data <- data.frame(ethanol = ethanol, h2 = h2_data(data, geno_m, relatedness_m), sig_h2 = h2_sig(data, geno_m, number_of_permutations, alpha, relatedness_m))
  print(data)
  data[2:5,] %>%
    ggplot(aes(ethanol, h2))+
    geom_col()+
    geom_errorbar(aes(ymin = sig_h2, ymax = sig_h2), size = 1.75, col = "red", alpha = 0.6)+
    labs(x = "Ethanol concentration (mM)", y = "h2")
}
    
h2_data(data, geno_m, relatedness_m)
h2_sig(data, geno_m, 1000, 0.05, relatedness_m)
h2_plot(data, geno_m, c(0, 100, 200, 300, 400), 1000, 0.05, relatedness_m)
 
