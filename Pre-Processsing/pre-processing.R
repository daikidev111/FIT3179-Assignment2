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
setwd("~/Desktop/FIT3179-Assignment2/Datasets")
rm(list=ls())

gdp_covid_df <- read.csv("gdp_per_capita_us.csv")
unemp_df <- read_excel("unemployment.xls")
gni_df <- read.csv("GNI.csv")
gdp_df <- read.csv("GDP_per_capita.csv")
covid_df <- read.csv("covid-data.csv")

#https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?view=chart
path_to_write <- "~/Desktop/FIT3179-Assignment2/Cleaned_Datasets/"

########## Data cleansing ###########
gdp_df <- gdp_df %>% clean_names()
gdp_df <- gdp_df %>% na.omit(gdp_df)
unemp_df <- unemp_df %>% clean_names()
unemp_df <- unemp_df %>% na.omit(unemp_df)
gni_df <- gni_df %>% na.omit(gni_df)

covid_df <- covid_df %>% clean_names()
covid_df <- covid_df %>% na.omit(covid_df)

gdp_covid_df <- gdp_covid_df %>% na.omit(gdp_covid_df)
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
#write.csv(df, paste(path_to_write, "economics.csv"), row.names=FALSE)

# countries selected for the analysis
#"China"
#"Japan"
#"Germany"
#"United Kingdom"
#"India"
#"France"




########## Data manipulation for covid-19 and GDP  ####
colnames(covid_df)[1] = "country"
covid_df <- subset(covid_df, covid_df$country == "United States")
covid_df$date <- substring(covid_df$date, 1, 4)
c.2020 = sum(covid_df$new_cases[covid_df$date == "2020"])
c.2021 = sum(covid_df$new_cases[covid_df$date == "2021"])

colnames(gdp_covid_df)[1] = "country"
gdp_covid_df <-subset(gdp_covid_df, gdp_covid_df$country == "United States")

df <- data.frame(
 country = rep(c("United States"), times=c(10)),
 year = rep(c(2012:2021), each=3),
 category = rep(c("GDP Per Capita", "COVID-19 New Cases", "Inflation Rate")),
 value= sample(0:2500, 30, replace=TRUE)
)


gdp_per_capita_us = scale(c(51784, 53291, 55124, 56763, 57867, 59915, 62805, 65095, 63028, 69288))
covid_new_case = scale(c(0, 0, 0, 0, 0, 0, 0, 0, c.2020, c.2021))
inflation_rate = scale(c(1.7, 1.5, 0.8, 0.7, 2.1, 2.1, 1.9, 2.3, 1.4, 7))

j = 1
k = 1 
l = 1
for (i in 1:nrow(df)) {
  if (i %% 3 == 1) {
    df[i, 4] = gdp_per_capita_us[j]
    print(gdp_per_capita_us[i])
    print(df[i, 4])
    j = j + 1
  } else if (i %% 3 == 2) {
    df[i, 4] = covid_new_case[k] 
    k = k + 1
  } else if (i %% 3 == 0) {
    df[i, 4] = inflation_rate[l] 
    l = l + 1
  } else {
    print("Error") 
    print(i)
  }
}

# normalization function for numerical variables
#normalization <- function(c) {
#  return((c-min(c))/(max(c)-min(c)))  
#}
#df.norm <- apply(df[,c("gdp_per_capita_us", "covid_new_case", "inflation_rate")], 2, normalization)
#df$gdp_normalised = c(df.norm[, 1])
#df$covid_new_case_normalised = c(df.norm[, 2])
#df$inflation_normalised = c(df.norm[, 3])

write.csv(df, paste(path_to_write, "three-line-chart.csv"), row.names=FALSE)


# Heatmap i
#install.packages("corrr")
library(corrr)
heat_df <- read.csv("final_demographics_data.csv")
heat_df <- select(heat_df, -c(1:2))
heat_df <- data.frame(apply(heat_df, 2, as.numeric))
corr_mat <- round(cor(heat_df),2)
corr_mat <- as.data.frame(corr_mat) 
corr_mat$y <- colnames(corr_mat)
corr_mat$x <- rownames(corr_mat)
write.csv(corr_mat, paste(path_to_write, "correlation_map.csv"), row.names=TRUE)


# pie chart
ind_gdp_df <- data.frame(
  industry_type = c("Finance", "Business Service", "Government", "Manufacturing", "Education, HealthCare", "Wholesale Trade", "Retail Trade", "Information", "Construction", "Others"),
  gdp_percentage = c(22.3, 12.8, 12.6, 10.8, 8.6, 5.8, 5.7, 5.5, 4.3, 11.6)
)
write.csv(ind_gdp_df, paste(path_to_write, "industry_gdp_us_2020.csv"), row.names = FALSE)
