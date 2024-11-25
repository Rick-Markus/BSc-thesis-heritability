library(tidyverse)
library(lmerTest) 

results <- read.csv2("C:\\Users\\Gebruiker\\Wageningen University & Research\\Lisa van Sluijs - Rick Markus\\Research\\External data\\counting data.csv", header = T)

results[,2] <- as.factor(results[, 2])
results[,1] <- as.numeric(results[,1])

#sample is een random effect  en time counted is een fixed effect
model <- lmer(size ~ replicate + (1|sample), REML = T, data = results)
summary(model) 
#count does not significantly change over time 

#check voor assumptions
plot(model)
#equal variances invalid

#normality is valid
for (sample in unique(results$sample)) {
  results_sample <- results$size[results$sample == sample]
  qqnorm(results_sample)
  qqline(results_sample)
}


mean <- aggregate(size~ sample, FUN = mean, data = results) 
sd <- aggregate(size ~ sample, FUN = sd, data = results)
sub_results <- cbind(mean, sd[, 2])
colnames(sub_results) <- c("sample", "mean", "sd")

sub_results$sd_nor <- sub_results$sd/sub_results$mean

simple_model <- lm(sd ~ mean, sub_results)
summary(simple_model) #mean sig; sd neemt toe voor grotere populaties

plot(simple_model)

sub_results %>%
  ggplot(aes(mean, sd))+
  geom_point()+
  geom_smooth(method = "lm", se = T, size = 0.8)+
  labs(x = "Estimated population size", y= "Standard deviation")

simple_model_nor <- lm(sd_nor ~ mean, data= sub_results)
summary(simple_model_nor) #mean niet sig: sd_nor naar pop size blijft gelijk; e.g. percentage afwijking is hetzelfde. 

plot(simple_model_nor)

sub_results %>%
  ggplot(aes(mean, sd_nor))+
  geom_point()+
  geom_smooth(method="lm", se = T, size = 0.8)+
  labs(x = "Estimated population size", y = "Normalized standard deviation")

mean(sub_results[,"sd_nor"]) 
#0.065 procent van totale sample size