# Loading libraries
library("tidyverse")
library("here")
library("janitor")
library("skimr")
library("dplyr")
library("Tmisc")
library("stats")
library("stargazer")
library("writexl")
library("plm")
library("ggplot2")
library("scales")
library("fastDummies")

#Loading Data
unemp <- read.csv(file.choose(), header = TRUE)

#Inspecting Data
colnames(unemp)
glimpse(unemp)
head(unemp)
skim_without_charts(unemp)

# Renaming Relevant variables
unemp <- rename(unemp, "Year"= "yr",
                "Unemployment_Rate" = "unemployment",
                "Int_Migrantion" = "migrant",
                "GDP_Growth_Rate" = "Gdp.growth..annual...",
                "Expenditure_on_Education" = "Government.on.education..total....of.GDP.",
                "Educational_Attainment" = "Educational.attainment",
                "Inflation_Rate" = "consumer.prices..annual...",
                "Government_Effectiveness" = "Government.Effectiveness..Estimate",
                "Political_Stability" = "political.Stability",
                "Population_Growth_Rate" = "Population.growth..annual...",
                "Political_Party" = "Political.Party",
                "Election_Year" = "Election.Year"
)

view(unemp)

# Checking for missing data in data set
unemp %>%
  map(~sum(is.na(.)))

#Dropping Columns with no data
unemp1 <- unemp %>%
  select(-c(Int_Migrantion,Educational_Attainment))

view(unemp1)

#Plotting the trend of unemployment
ggplot(data=unemp1)+
  geom_smooth(mapping=aes(x=Year, y=Unemployment_Rate))+
  labs(title = "Trend of Unemployment Rate")

# Creating a data frame with no missing values
unemp11 <- unemp1 %>%
  na.omit()

#Creating dummy variables for Party and Election years
unemp11 <- dummy_cols(unemp11,remove_first_dummy = TRUE)

view(unemp11)

#Running Simple OLS Regression
Simple_Reg <- lm(Unemployment_Rate ~ GDP_Growth_Rate + Expenditure_on_Education + 
                   Inflation_Rate + Government_Effectiveness + Political_Stability +
                   Population_Growth_Rate + Political_Party_Republican + Election_Year_Yes,data=unemp11)

summary(Simple_Reg)
stargazer(Simple_Reg)
