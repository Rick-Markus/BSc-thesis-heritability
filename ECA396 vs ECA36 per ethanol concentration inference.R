library(car)

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

results_strains <- results[results$strain == "ECA396" | results$strain == "ECA36",]
sub_results <- results_strains[results_strains$ethanol != 0,]

#100 mM
leveneTest(sub_results$broodsize_nor[sub_results$ethanol == 100] ~ sub_results$strain[sub_results$ethanol == 100]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 100 & sub_results$strain == "ECA396"]) #non normal
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 100 & sub_results$strain == "ECA36"]) #non normal
wilcox.test(sub_results$broodsize_nor[sub_results$ethanol == 100 & sub_results$strain == "ECA396"], sub_results$broodsize_nor[sub_results$ethanol == 100 & sub_results$strain == "ECA36"], exact = F)
# p = 1.63e-05

#200 mM
leveneTest(sub_results$broodsize_nor[sub_results$ethanol == 200] ~ sub_results$strain[sub_results$ethanol == 200]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 200 & sub_results$strain == "ECA396"]) #normal
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 200 & sub_results$strain == "ECA36"]) #non normal
wilcox.test(sub_results$broodsize_nor[sub_results$ethanol == 200 & sub_results$strain == "ECA396"], sub_results$broodsize_nor[sub_results$ethanol == 200 & sub_results$strain == "ECA36"], exact = F)
# p = 0.0001361

#300 mM # EQUAL VARIANCE ASSUMPTION IS NOT TRUE!!!!!! 
leveneTest(sub_results$broodsize_nor[sub_results$ethanol == 300] ~ sub_results$strain[sub_results$ethanol == 300]) # non equal var
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 300 & sub_results$strain == "ECA396"]) #non normal
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 300 & sub_results$strain == "ECA36"]) #non normal
wilcox.test(sub_results$broodsize_nor[sub_results$ethanol == 300 & sub_results$strain == "ECA396"], sub_results$broodsize_nor[sub_results$ethanol == 300 & sub_results$strain == "ECA36"], exact = F)
#p = 2.985e-06

#400 mM
leveneTest(sub_results$broodsize_nor[sub_results$ethanol == 400] ~ sub_results$strain[sub_results$ethanol == 400]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 400 & sub_results$strain == "ECA396"]) #non normal
shapiro.test(sub_results$broodsize_nor[sub_results$ethanol == 400 & sub_results$strain == "ECA36"]) #non normal
wilcox.test(sub_results$broodsize_nor[sub_results$ethanol == 400 & sub_results$strain == "ECA396"], sub_results$broodsize_nor[sub_results$ethanol == 400 & sub_results$strain == "ECA36"], exact = F)
#0.003665