library(tidyverse)
library(dplyr)
print("data charged")
data <- read.csv("DATA/Crime_Data_from_2020_to_Present.csv", header = TRUE)
print("data load")

titre_appli = " analyse crime 2023"

variables_data = names(data)


data$DATE.OCC <- as.Date(data$DATE.OCC,format = "%m/%d/%Y")
data$month<- lubridate::month(data$DATE.OCC, label = TRUE) 


data$year<- lubridate::year(data$DATE.OCC)
data$year<-as.factor(data$year)
