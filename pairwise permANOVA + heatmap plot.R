library(devtools)
library(pheatmap)

install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

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

#pairwise comparisons
#Change subset results$ethanol for different ethanol concentrations
#100 --> set.seed(2) 
#200 --> set.seed(3)
#300 --> set.seed(4)
#400 --> set.seed(5)
#3500 permutations for bonferroni correction to still be able to significant
sub_results <- results[results$ethanol == 400,]
sub_dist_matrix <- dist(sub_results$broodsize_nor, method = "euclidean")
set.seed(5)
pair_outcome_good <- pairwise.adonis2(x = sub_dist_matrix ~ strain_no, data = sub_results, permutations = 3500)
print(pair_outcome_good)


#extract p values
data <- pair_outcome_good

#to store p values
p_value_matrix <- matrix(NA, nrow = 16, ncol = 16)

strains <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")

#for loop to extract P values and fill bottom half of matrix
for (i in 1:(length(strains) - 1)) {
  for (j in (i + 1):length(strains)) {
    # Create the comparison names for both directions
    comparison_name_1 <- paste(strains[i], "vs", strains[j], sep = "_")
    comparison_name_2 <- paste(strains[j], "vs", strains[i], sep = "_")
    
    # Check if either comparison exists in the list
    if (comparison_name_1 %in% names(pair_outcome_good)) {
      # Extract the p-value for comparison_name_1
      p_value <- pair_outcome_good[[comparison_name_1]]$`Pr(>F)`[1]  # Assuming the p-value is in the first row
    } else if (comparison_name_2 %in% names(pair_outcome_good)) {
      # Extract the p-value for comparison_name_2
      p_value <- pair_outcome_good[[comparison_name_2]]$`Pr(>F)`[1]  # Assuming the p-value is in the first row
    } else {
      # If neither comparison exists, set p-value to NA
      p_value <- NA
    }
    
    # Fill in the matrix with the p-value
    p_value_matrix[j, i] <- p_value
  }
}


#make a heatmap of p values
data <- p_value_matrix

#bonferroni corrected p-values
number_of_comparisons = 120 #(15+14+....+1)
data <- data * number_of_comparisons
data[data > 1] <- 1

# Prep function to adjust matrix for heatmap visualization
prep_data <- function(matrix) {
  heatmap_matrix <- matrix
  heatmap_matrix[heatmap_matrix > 0.05] <- 0.06  # Set NS values to 0.99 (indicating non-significant)
  diag(heatmap_matrix) <- NA  # Keep diagonals as NA for distinct color (grey)
  return(heatmap_matrix)
}

#make the heatmap in desired format
heatmap_data <- prep_data(data)

# Define color palette for heatmap
colors <- c("blue", "white")  # Gradient for significant values, white for NS

# Convert heatmap_data to numeric matrix
heatmap_data <- apply(heatmap_data, 2, as.numeric)

colnames(heatmap_data) <- c("CB4856", "DL238", "ECA36", "ECA396", "EG4725", "JU1400", "JU2526", "JU2600", "JU310", "MY2147", "MY2693", "N2", "NIC2", "NIC526", "QX1794", "XZ1516")
rownames(heatmap_data) <- c("CB4856", "DL238", "ECA36", "ECA396", "EG4725", "JU1400", "JU2526", "JU2600", "JU310", "MY2147", "MY2693", "N2", "NIC2", "NIC526", "QX1794", "XZ1516")

# Define breaks to match colors with thresholds
breaks <- c(seq(0, 0.05, length.out = 50), 0.06)  # 51 values for gradient, 0.99 for NS, 1 for NA

pheatmap(heatmap_data,
         color = colors,
         display_numbers = FALSE,
         cellwidth = 20,
         cellheight = 15,
         na_col = "grey",         # NA values in grey
         cluster_rows = F,    # Disable clustering
         cluster_cols = F,
         legend = F)
