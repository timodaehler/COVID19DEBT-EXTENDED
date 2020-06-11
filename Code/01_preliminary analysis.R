#Author: Timo Daehler, daehler@usc.edu
#Date of last update: 28 April 2020
#Purpose: preliminary analysis and choice of countries that should be in the sample
#Inputs: -
#Outputs: figures
#Other relevant notes: 


#loading required packages
install.packages(c("dplyr", "readxl", "rworldmap"))
library(readxl)
library(dplyr)
library(rworldmap)

#importing the IMF.xls sheet, which contains all country for which Debt/GDP in 2018 is avaiable
imfDF <- read_excel("/Users/timodaehler/Desktop/COVID19DEBT/Data/IMF.xls", sheet = 1)  

#Format of debt ratio needs to be changed to numeric
imfDF$Debt <- as.numeric(imfDF$Debt) 

#filtering for EM countries and creating dataframe that only contains emerging markets
EMOnlyDF <-  imfDF %>% filter(EM == 1) 

#counting number of countries in dataframe
nrow(EMOnlyDF)
#88 countries are classified as developing or emerging market

#filtering for emerging markets with debt ratio above threshold
threshold <- 20
EMOnlyDF %>% filter(Debt > threshold)
#77 of 88 EM countries have Debt/GDP above 20%. 

#I increase the threshold to 50%
threshold <- 50
EMOnlyDF %>% filter(Debt > threshold)
#41 of 88 EM countries have Debt/GDP above 50%. Many of them are countries from Africa, South America,
#the Carribean or Central Asian. While there is of course some overlap, crucial countries such as
#China, India, and Brazil would not be in the sample. I conclude that the EMBI constituents should
#be used instead of a threshold approach. 

#In the next step I visualize the EMBI countries.

#Creating the vector of the 31 countries in the EMBI
EMBICountries <- c("IDN", "MEX", "SAU", "RUS", "QAT", "PHL", "CHN", "TUR", "ARE", "BRA", "COL", "KAZ", "CHL", "ZAF", "PER", "PAN", "URY", "DOM", "EGY", "BHR", "OMN", "UKR", "MYS", "HUN", "LKA", "NGA", "POL", "GHA", "ARG", "AZE", "ROU")
#Checking if there are 31 countries in the EMBI vector 
length(EMBICountries) == 31
#Check result: positive

#Creating the vector of the countries I would add on top (India, Thailand)
additionalCountries <- c("IND", "THA")

#Combining all countries
allCountriesDF <- c(EMBICountries, additionalCountries)

# allCountriesDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data. The variable is 1 for EMBI countries and 2 for additional countries
allCountriesDF <- data.frame(country = allCountriesDF,
                             EMBI = c(rep.int(1, 31), rep.int(2,2))  )

# This will join the allCountriesDF data.frame to the country map data
allCountriesMap <- joinCountryData2Map(allCountriesDF, joinCode = "ISO3",
                                       nameJoinColumn = "country")

# And this will plot it, with the trick that the color palette's first
# color is red
mapCountryData(allCountriesMap, nameColumnToPlot="EMBI", catMethod = "categorical",
               missingCountryCol = gray(.8), addLegend = FALSE, mapTitle = "EMBI+2 (Thailand & India)")






