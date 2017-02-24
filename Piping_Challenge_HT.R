# TeamShrub piping challenge
# Calculate the LPI for each country within the LPI database
# Data available from http://www.livingplanetindex.org/home/index

# 24-02-2016

# Libraries ----
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(readr)
library(broom)

# Load data ----
data<-read.csv(file="LPIdata_Feb2016.csv")

#Reshape data
data <- data %>%
  gather("Year", "Pop", 26:70)

# Calculate country-specific LPI ----
test<- data %>%
  mutate(Year = parse_number(Year)) %>%
  filter(!is.na(Pop) & Are.coordinates.for.specific.location.=="TRUE") %>%
  select(Common.Name,Location.of.population,Country.list,Year,Pop,system,Native,Alien) %>%
  group_by(Common.Name,Location.of.population,Country.list,system,Native,Alien) %>%
  filter(length(unique(Year)) > 2) %>%
  do(fit = lm(Pop ~ Year, data = .)) %>%
  tidy(fit) %>%
  ungroup() %>%
  group_by(Country.list) %>%
  do(ggsave(ggplot(.,aes(x = estimate))+
              geom_histogram()),filename = paste("Country_LPI/",unique(.$Country.list),".pdf",sep=""),device="pdf")

ggplot(test,aes(estimate))+
  geom_histogram()
  
?ggsave
# Visualise LPI for each country and save the plots ----
# Loops or pipes? Can you save individual files for each plot and create panels with e.g. 6 countries per panel?