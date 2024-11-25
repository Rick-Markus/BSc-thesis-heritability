library(tidyverse)

data_wide <- read.table("C:\\Users\\Gebruiker\\Wageningen University & Research\\Lisa van Sluijs - Rick Markus\\Research\\Analysis\\Data analysis results Marijke\\CSV files analysis\\ethanol evaporation data .csv", header = T, sep = ";")

data <- data_wide %>%
  pivot_longer(cols = c(`X0`, `X15`, `X30`, `X45`, `X60`, `X75`, `X90`, `X105`, `X120`, `X135`, `X150`, 
                 `X165`, `X180`, `X195`, `X210`, `X225`, `X240`), 
               names_to = "time", 
               values_to = "eth_vol"
  )

data$time <- sub("^X", "", data$time)

data$time <- as.numeric(data$time)
data$volume <- as.factor(data$volume)

attach(data)
#make a subdata frame where that stops time after two subsequent steps show less than 2 percent increase
sub_data <- data[(volume == 29.2 & time <= 90) | (volume == 58.4 & time <= 195)| (volume == 87.6 & time <= 240)| (volume == 116.8 & time <= 165),]

sub_data %>% 
  ggplot(aes(time, eth_vol, color = volume))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~poly(x,2), se = F, size = 0.6)+
  labs(x = "Time (min)", y = "Ethanol evaporated (μL)")+
  scale_color_manual(values = c("29.2" = "red", "58.4" = "green", "87.6" = "blue", "116.8" = "purple"), 
                     labels = c("29.2" = "29.2 μL", "58.4" = "58.4 μL", "87.6" = "87.6 μL", "116.8" = "116.8 μL"))
sub_data %>% 
  ggplot(aes(time, eth_vol, color = volume))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, size = 0.7)+
  labs(x = "Time (min)", y = "Ethanol evaporation (μL)")+
  facet_wrap(~ volume, labeller = labeller(volume = c("29.2" = "29.2 μL", "58.4" = "58.4 μL", "87.6" = "87.6 μL", "116.8" = "116.8 μL")))

sub_data$time2 <- (sub_data$time)^2
sub_data$time3 <- (sub_data$time)^3

#subdata
m_vol_1_s <- lm(eth_vol ~ time + time2, data = sub_data[sub_data$volume == "29.2",])
m_vol_2_s <- lm(eth_vol ~time + time2, data = sub_data[sub_data$volume == "58.4",])
m_vol_3_s <- lm(eth_vol ~time + time2, data = sub_data[sub_data$volume == "87.6",])
m_vol_4_s <- lm(eth_vol ~ time + time2, data = sub_data[sub_data$volume == "116.8",])

summary(m_vol_1_s) #29.2
summary(m_vol_2_s)
summary(m_vol_3_s)
summary(m_vol_4_s)

# assumpties nog checken als het kan