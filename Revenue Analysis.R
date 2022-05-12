#Load Libraries
library("ggplot2")
library("here")
library("skimr")
library("janitor")
library("dplyr")
library("palmerpenguins")
library("tidyverse")
library("SimDesign")
library("tinytex")

#Load Data
Revenue <- read.csv(file.choose(),header=TRUE)

#Checking column names
colnames(Revenue)

#Renaming column names
Revenue <- Revenue %>%
  rename(Year=YEAR,Total_Revenue=Total.Revenue,Tax_Revenue=Tax.Revenue,
         CIT=Corporate.Income.Tax,Income_Tax=Income.Tax.Revenue,
         PIT=Individual.Income.Tax.Revenue,VAT=VAT.Revenue,
         Trade_Tax=Trade.Tax.Revenue,Goods_Services_Tax=Goods.and.Services.Tax,
         Excise_Tax=Excise.Tax)

#Rechecking Column names
colnames(Revenue)

#Inspecting Data
head(Revenue)
glimpse(Revenue)
str(Revenue)

#Summary statistics (Mean)
Mean_of_Revenue_Measures <- Revenue %>%
  summarise(Mean_Total_Revenue=mean(Total_Revenue),Mean_Tax_Revenue=mean(Tax_Revenue),
            Mean_CIT=mean(CIT),Mean_Income_Tax=mean(Income_Tax),Mean_PIT=mean(PIT),
            Mean_VAT=mean(VAT),Mean_Trade_Tax=mean(Trade_Tax),
            Mean_GST=mean(Goods_Services_Tax),
            Mean_Excise_Tax=mean(Excise_Tax))

#Viewing mean values
view(Mean_of_Revenue_Measures)

#Plotting Total Revenue
ggplot(data=Revenue)+
  geom_line(mapping=aes(x=Year,y=Total_Revenue),color="red")+
  labs(title = "Total Revenue")
  
#Plotting Tax Revenue
ggplot(data=Revenue)+
  geom_line(mapping=aes(x=Year,y=Tax_Revenue),color="green")+
  labs(title = "Total Tax Revenue") 

#Plotting Relationship between Tax and Total revenue
ggplot(data=Revenue)+
geom_point(mapping=aes(x=Tax_Revenue,y=Total_Revenue),color="Red")+
  labs(title="Tax and Total Revenue Association")

#Linear Regression of Determinants of Tax Revenue
OLS <- lm(Tax_Revenue~CIT+PIT+VAT+Trade_Tax+Goods_Services_Tax+Excise_Tax,data=Revenue)

summary(OLS)

