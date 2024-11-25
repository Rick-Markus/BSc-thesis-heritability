#Execute the Anova_table function in the ANOVA type 2 SS table script
#no normality; no equal variance
library(tidyverse)
library(dplyr)
library(car)

results <- read.csv("C:/Users/Gebruiker/Wageningen University & Research/Lisa van Sluijs - Rick Markus/Research/Analysis/Data analysis results Marijke/CSV files analysis/Ethanol-Broodsize data (non NA).csv", header = T)
results$strain <- as.factor(results$strain) 
results$ethanol <- as.factor(results$ethanol)
results$strain_no <- as.factor(results$strain_no)
results$replicate <- as.factor(results$replicate)
results$super_replicate <- as.factor(results$super_replicate)
results$broodsize_nor <- as.numeric(results$broodsize_nor)

#1 for data Rick; 2 for data Marijke #remove filter for combined data
results <- results[results$super_replicate == 1, ]

model <- lm(broodsize_nor ~ strain + ethanol + strain:ethanol, results)
Anova_table(model)

attach(results)

result <- aggregate(broodsize_nor ~ strain + ethanol, results, mean)


SE <- aggregate(broodsize_nor ~ ethanol + strain, results, sd)
result <- cbind(result[, 1:3], SE[ , 3])
colnames(result) <- c("strain", "ethanol", "broodsize_nor", "SE")

result %>%
  ggplot(aes(x = ethanol, y = broodsize_nor, group = strain))+ 
  geom_jitter(alpha = 0.5, size = 2, width = 0.05, color = "blue")+
  geom_line(alpha = 0.3, size = 1.2, color = "blue")+
  geom_errorbar(aes(ymin = broodsize_nor-SE, ymax = broodsize_nor+SE), alpha = 0.3, position = position_jitter(width = 0.05), width = 0.01, size = .9, color = "blue")+
  facet_wrap(~ strain, labeller = labeller(strain = c("1" = "CB4856", "2" = "DL238", "3" = "ECA36", "4" = "ECA396", "5" = "EG4725", "6" = "JU1400", "7" = "JU2526", "8" = "JU2600", "9" = "JU310", "10" = "MY2147", "11" = "MY2693", "12" = "N2", "13" = "NIC2", "14" = "NIC526", "15" = "QX1794", "16" = "XZ1516")))+ 
  labs(title = "Brood size-ethanol experiment", x = "ethanol concentration (mM)", y = "normalized brood size")+
  scale_color_discrete(labels = c("CB4856", "DL238", "ECA36", "ECA396", "EG4725", "JU1400", "JU2526", "JU2600", "JU310", "MY2147", "MY2693", "N2", "NIC2", "NIC526", "QX1794", "XZ1516"))

#normality assumption
eij <- residuals(model) 
results <- na.omit(results)
results <- cbind(results, eij)
qqnorm(eij, main = "check normality assumption")
qqline(eij, col = "red") 

shapiro.test(eij)

#equal variance assumption
strain_ethanol <- interaction(results$strain, results$ethanol)
results <- cbind(results, strain_ethanol)
results %>% 
  ggplot(aes(strain_ethanol, eij))+ 
  geom_point(aes(color = strain_ethanol))+
  geom_hline(yintercept = 0, color = 'black')+
  theme(legend.position = "none") 

leveneTest(eij ~ strain_ethanol) 

             
