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
data <-read.csv(file="LPIdata_Feb2016.csv")

data <- data[-3796,]
data <- data[-3798,]
data <- data[-3825,]
data <- data[-4193,]
data <- data[-7886,]
data <- data[-13101,]
data <- data[-14354,]
data <- data[-15310,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-15327,]
data <- data[-16412,]

gsub(" ","",data$Country.list, fixed = TRUE)

#Reshape data
data <- data %>%
  gather("Year", "Pop", 26:70)

# Calculate country-specific LPI ----
test <- data %>%
  mutate(Year = parse_number(Year)) %>%
  mutate(Pop = parse_number(Pop)) %>%
  filter(!is.na(Pop) & Are.coordinates.for.specific.location.=="TRUE") %>%
  select(Common.Name,Location.of.population,Country.list,Year,Pop,system,Native,Alien) %>%
  group_by(Common.Name,Location.of.population,Country.list,system,Native,Alien) %>%
  filter(length(unique(Year)) > 2) %>%
  do(fit = lm(Pop ~ Year, data = .)) %>%
  tidy(fit) %>%
  ungroup() %>%
  group_by(Country.list) %>%
  do(ggsave(ggplot(.,aes(x = estimate))+geom_histogram(),filename = gsub(" ","",paste("Country_LPI/",unique(as.character(.$Country.list)),".pdf",sep="")),device="pdf"))

# Visualise LPI for each country and save the plots ----
# Loops or pipes? Can you save individual files for each plot and create panels with e.g. 6 countries per panel?