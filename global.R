library(shinyWidgets)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(maps)
library(mapproj)

# Loading the data and specifying empty places change to NA
EV <- read.csv("EV.csv",header = T,na.strings=c("","NA"))

# Removing the Date and Month from the date
EV$Year<- substring(EV$Submitted.Date, 7)

# Removing the First column()
EV <- EV[,-1]

# Changing Column Names
colnames(EV)[1] <- "Date"

EV$Date<-mdy(EV$Date)

EV$Month_Yr <- format(as.Date(EV$Date), "%Y-%m")


# Checking for NA Values 
colnames(EV)[colSums(is.na(EV)) > 0]

# Filling the NA values of County through Zip from other columns
EV<-EV %>%
  group_by(ZIP) %>% 
  fill(County, .direction = "downup")

# Dropping any other NA values from other columns
EV<- EV %>% drop_na()

# Making sure there are No NA Values 
colnames(EV)[colSums(is.na(EV)) > 0]

# Converting the County names to lower case
EV$County <- tolower(EV$County)

# Changing column names of 8 and 9 Columns
colnames(EV)[8] <- "CO2"
colnames(EV)[9] <- "Petrol"
colnames(EV)[10] <- "Rebate_Amount"

# creating a column name by combining the make and model
EV$Name <- paste(EV$Make, "-", EV$Model)


# Loading the New york County wise Map data
states <- map_data("state")
ny_df <- subset(states, region == "new york")
counties <- map_data("county")
ny_county <- subset(counties, region == "new york")
colnames(ny_county)[6]<-"County"
NY_County_map <- ggplot(data = ny_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")+
  geom_polygon(data = ny_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank()
  )

Petrol <- EV%>%
  select(Year,Petrol)%>%
  group_by(Year)%>%
  summarise(Petrol_Gallons = sum(Petrol))

CO2 <- EV%>%
  select(Year,CO2)%>%
  group_by(Year)%>%
  summarise(MTCO2E = sum(CO2))

Rebate <- EV%>%
  select(Year,Rebate_Amount)%>%
  group_by(Year)%>%
  summarise(Rebate_Amount = sum(Rebate_Amount))  

Stats <- merge(Petrol, CO2) %>%
  merge(Rebate)
  