#Load Libraries
library("tidyverse")
library("here")
library("janitor")
library("dplyr")
library("Tmisc")
library("stats")
library("stargazer")
library("writexl")
library("plm")

#Load Data
InfData <- read.csv(file.choose(),header=TRUE)

#Inspecting Data
View(InfData)
colnames(InfData)
head(InfData)
str(InfData)
glimpse(InfData)

#Checking for Missing data
InfData %>%
  map(~sum(is.na(.)))

#Dropping Missing Values
 InfData2 <- InfData %>%
  drop_na()
 
 InfData2 %>%
   map(~sum(is.na(.)))
 
 #Summary Statistics (Means)
 Mean_Data <- InfData2 %>%
   group_by(Country) %>%
   summarise(across(c(Tax_Revenue,Income_Tax,Income_TTax,Tax_Goods_Services,Tax_IntTrade,Inflation,FDI,GDP_Per_Capita,Manufacturing,Labor_Force,Unemployment,Trade),mean),
             .groups = 'drop')
    
 View(Mean_Data)   
 
 #Exporting Summary stats 
 write_xlsx(Mean_Data,"Desktop/DataBase.xlsx")
 
#Starting Panel regression (Pooling Data)
 attach(InfData2)
 Y <- cbind(Income_Tax)
 Y2 <- cbind(Tax_Goods_Services)
 Y3 <- cbind(Tax_IntTrade)
 Y4 <- cbind(Tax_Revenue)
 X <- cbind(Inflation,FDI,GDP_Per_Capita,Manufacturing,Labor_Force,Unemployment,Trade)
 
 #Summary of Pooled Data
 summary(X)
 summary(Y)
 summary(Y2)
 summary(Y3)
 summary(Y4)
 
 #Declaring Pooled Data (Dependent variable: Income Ta )
 PYData <- plm.data(InfData2, index=c("Country","Year"))
 
 #Pooled OLS for Dependent Variables
 OLS_Income_Tax <- plm(Y~X, data = PYData, model = "pooling")
 summary(OLS_Income_Tax)
 
 OLS_Goods_Services <- plm(Y2 ~ X, data = PYData,model = "pooling")
 summary(OLS_Goods_Services)
 
 OLS_Int_Trade <- plm(Y3 ~ X, data = PYData,model = "pooling")
 summary(OLS_Int_Trade)
 
 OLS_Tax_Rev <- plm(Y4 ~ X, data = PYData,model = "pooling")
 summary(OLS_Tax_Rev)
 
 #Fixed Effect Estimator
 FE_Income_Tax <- plm(Y~X, data = PYData, model = "within")
 summary(FE_Income_Tax)
 
 FE_Goods_Services <- plm(Y2~X, data = PYData, model = "within")
 summary(FE_Goods_Services)
 
 FE_Int_Trade <- plm(Y3~X, data = PYData, model = "within")
 summary(FE_Int_Trade)
 
 FE_Tax_Rev <- plm(Y4~X, data = PYData, model = "within")
 summary(FE_Tax_Rev)
 
 #Random
 RE_Income_Tax <- plm(Y2~X, data = PYData, model = "random")
 summary(RE_Income_Tax)
 
 