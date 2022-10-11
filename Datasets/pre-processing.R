#################### Assignment Information ################################
# Author: Daiki Kubo
# Student ID: 30523346
# Title: FIT3179 Week 9 Homework

################################### Libraries ###############################
library(janitor)
library(tidyr)
library(data.table) 
library(dplyr) 
library(lubridate)
library("readxl")
#install.packages("readxl")
#install.packages("xlsx")
library("xlsx")
################################### Project Setup ############################
setwd("~/Desktop/vega-lite-hw/data")
rm(list=ls())
#glob_temp <- read.csv("GlobalLandTemperaturesByCountry.csv")
#glob_temp_path <- "GlobalLandTemperaturesByCountry.csv"

gdp_df <- read_excel("gdp_data.xls")
unemp_df <- read_excel("unemployment.xls")
gni_df <- read.csv("GNI.csv")
gdp_df <- read.csv("GDP_per_capita.csv")

#https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?view=chart
path_to_write <- "~/Desktop/vega-lite-hw/cleaned-data/"

########## Data cleansing ###########
#glob_temp <- glob_temp %>% clean_names()
#glob_temp <- na.omit(glob_temp)
gdp_df <- gdp_df %>% clean_names()
gdp_df <- gdp_df %>% na.omit(gdp_df)
unemp_df <- unemp_df %>% clean_names()
unemp_df <- unemp_df %>% na.omit(unemp_df)
gni_df <- gni_df %>% na.omit(gni_df)

##### Data manipulation #############
#glob_temp <- glob_temp[-c(3)]
# extracting a year
#glob_temp$dt <- substring(glob_temp$dt, 1, 4)
#glob_temp <- subset(glob_temp, glob_temp$dt == 2013)
#write.csv(glob_temp, paste(path_to_write, glob_temp_path), row.names = FALSE)

#### DATA MANIPULATION #####
colnames(gdp_df)[1] = "country"
colnames(unemp_df)[1] = "country"
colnames(gni_df)[1] = "country"
#gdp_df = select(gdp_df, -c(2:40))
unemp_df = select(unemp_df, -c(2:40))
gdp_df = select(gdp_df, -c(3:10))
unemp_df = select(unemp_df, -c(3:10))
colnames(gdp_df)[2] = "2019_GDP"
colnames(unemp_df)[2] = "2019_UNEMP"
colnames(gni_df)[2] = "2019_GNI"
df = merge(gdp_df, unemp_df, by = "country")
df = merge(df, gni_df, by="country")

for (i in 1:nrow(df)) {
  if (df$`2019_GNI`[i] >= 13205) {
    df$Class[i] = "High Income"
  } else if (df$`2019_GNI`[i] <= 1085) {
    df$Class[i] = "Low Income"
  } else if (df$`2019_GNI`[i] > 1085 && df$`2019_GNI`[i] <= 4255) {
    df$Class[i] = "Lower Middle Income"
  } else {
    df$Class[i] = "Upper Middle Income"
  }
}

#change the datatype
df$`2019_GDP` = as.numeric(df$`2019_GDP`)
df$`2019_UNEMP` = as.numeric(df$`2019_UNEMP`)

for (i in 1:nrow(df)){
  df$`2019_GDP`[i] = round(df$`2019_GDP`[i], 2)
  df$`2019_UNEMP`[i] = round(df$`2019_UNEMP`[i], 2)
  df$`2019_GNI`[i] = round(df$`2019_GNI`[i], 2)
}


#gdp_df$country[37] = "China"
#gdp_df = gdp_df %>% filter((gdp_df$country == "United States") | (gdp_df$country == "China") | (gdp_df$country == "India") | (gdp_df$country == "United Kingdom") | (gdp_df$country == "Germany") | (gdp_df$country == "Japan") | (gdp_df$country == "France"))
write.csv(df, paste(path_to_write, "economics.csv"), row.names=FALSE)

# countries selected for the analysis
#"China"
#"Japan"
#"Germany"
#"United Kingdom"
#"India"
#"France"