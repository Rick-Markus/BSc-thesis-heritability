library(writexl)
library(readr)

data <- read.csv2("C:\\Users\\Gebruiker\\Wageningen University & Research\\Lisa van Sluijs - Rick Markus\\Research\\Analysis\\Data analysis results Marijke\\CSV files analysis\\Ethanol-Broodsize data.csv", header = T)


data <- na.omit(data)

data_nor_broodsize <- function(data) {
  for (strain in unique(data$strain)) {
    mean <- sum(data$broodsize[data$strain == strain & data$ethanol == 0]) / length(data$broodsize[data$strain == strain & data$ethanol == 0])
    data$broodsize_nor[data$strain == strain] <- data$broodsize[data$strain == strain] / mean   
  }
  return(data)
}

data <- data_nor_broodsize(data)
mean(data$broodsize_nor[data$strain == "DL238" & data$ethanol == 0])

write_csv(data, "C:\\Users\\Gebruiker\\Wageningen University & Research\\Lisa van Sluijs - Rick Markus\\Research\\Analysis\\Data analysis results Marijke\\CSV files analysis\\Ethanol-Broodsize data (non NA).csv")

