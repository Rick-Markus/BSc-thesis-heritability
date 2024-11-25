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
results <- results[results$super_replicate == 1,]

results <- data_nor_broodsize(results)
mean(results$broodsize_nor[results$strain == "DL238" & results$ethanol == 0])

results_0_100 <- results[results$ethanol == 0 | results$ethanol == 100,]
sub_results <- results_0_100[results_0_100$strain == "ECA396" | results_0_100$strain == "JU1400" | results_0_100$strain == "MY2147" | results_0_100$strain == "NIC526" | results_0_100$strain == "JU2600",]

#ECA396
leveneTest(sub_results$broodsize_nor[sub_results$strain == "ECA396"] ~ sub_results$ethanol[sub_results$strain == "ECA396"]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "ECA396" & sub_results$ethanol == 0]) #non normal
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "ECA396" & sub_results$ethanol == 100]) #non normal
wilcox.test(sub_results$broodsize_nor[sub_results$strain == "ECA396" & sub_results$ethanol == 100], sub_results$broodsize_nor[sub_results$strain == "ECA396" & sub_results$ethanol == 0], exact = F, alternative = "greater")
#0.01699

#JU1400
leveneTest(sub_results$broodsize_nor[sub_results$strain == "JU1400"] ~ sub_results$ethanol[sub_results$strain == "JU1400"]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "JU1400" & sub_results$ethanol == 0]) #non normal
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "JU1400" & sub_results$ethanol == 100]) #normal
wilcox.test(sub_results$broodsize_nor[sub_results$strain == "JU1400" & sub_results$ethanol == 100], sub_results$broodsize_nor[sub_results$strain == "JU1400" & sub_results$ethanol == 0], exact = F, alternative = "greater")
#0.9941 # laat zelfs significant decrease zien (bij two tailed test)

#JU2600
leveneTest(sub_results$broodsize_nor[sub_results$strain == "JU2600"] ~ sub_results$ethanol[sub_results$strain == "JU2600"]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "JU2600" & sub_results$ethanol == 0]) #normal
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "JU2600" & sub_results$ethanol == 100]) #normal
wilcox.test(sub_results$broodsize_nor[sub_results$strain == "JU2600" & sub_results$ethanol == 0], sub_results$broodsize_nor[sub_results$strain == "JU2600" & sub_results$ethanol == 100], exact = F)
t.test(sub_results$broodsize_nor[sub_results$strain == "JU2600" & sub_results$ethanol == 100], sub_results$broodsize_nor[sub_results$strain == "JU2600" & sub_results$ethanol == 0], var.equal = T, alternative = "greater")
#t test = 0.1771
#bonferroni corrected = 0.1771*16 = >1 = 1 :)...
#DEZE NOG BONFERRONI CORRIGEREN; OMDAT IN MIJN SAMPLE HOGER NIET IN VAN MARIJKE!!!!

#MY2147
leveneTest(sub_results$broodsize_nor[sub_results$strain == "MY2147"] ~ sub_results$ethanol[sub_results$strain == "MY2147"]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "MY2147" & sub_results$ethanol == 0]) #normal
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "MY2147" & sub_results$ethanol == 100]) #normal
wilcox.test(sub_results$broodsize_nor[sub_results$strain == "MY2147" & sub_results$ethanol == 0], sub_results$broodsize_nor[sub_results$strain == "MY2147" & sub_results$ethanol == 100], exact = F)
t.test(sub_results$broodsize_nor[sub_results$strain == "MY2147" & sub_results$ethanol == 100], sub_results$broodsize_nor[sub_results$strain == "MY2147" & sub_results$ethanol == 0], var.equal = T, alternative = "greater")
#0.6179

#NIC526
leveneTest(sub_results$broodsize_nor[sub_results$strain == "NIC526"] ~ sub_results$ethanol[sub_results$strain == "NIC526"]) #equal var
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "NIC526" & sub_results$ethanol == 0]) #normal
shapiro.test(sub_results$broodsize_nor[sub_results$strain == "NIC526" & sub_results$ethanol == 100]) #normal
wilcox.test(sub_results$broodsize_nor[sub_results$strain == "NIC526" & sub_results$ethanol == 0], sub_results$broodsize_nor[sub_results$strain == "NIC526" & sub_results$ethanol == 100], exact = F)
t.test(sub_results$broodsize_nor[sub_results$strain == "NIC526" & sub_results$ethanol == 100], sub_results$broodsize_nor[sub_results$strain == "NIC526" & sub_results$ethanol == 0], var.equal = T, alternative = "greater")
#t test = 0.7816