#Author: Timo Daehler, daehler@usc.edu
#Date of last update: 14 Mai 2020
#Purpose: preliminary look at the dataset to find out interesting phenomena 
#Inputs: -
#Outputs: Figures in folder in folder "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02"
#Other relevant notes: 



#loading required packages
install.packages("tidyverse")
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(readxl)
library(lubridate)

#importing the Data.xls sheet
rawDataDF <- read_excel("/Users/timodaehler/Desktop/COVID19DEBT/Data/Data.xlsx", sheet = 1)
View(rawDataDF)

#Converting all "NA" to empty values
rawDataDF[rawDataDF == "NA"] <- ""

#Viewing the data
View(rawDataDF)

#here I change the type of the variable whenever necessary
rawDataDF <- mutate(rawDataDF, 
                    COUNTRY = as.factor(COUNTRY),
                    IMF_COUNTRY_NAME = as.factor(IMF_COUNTRY_NAME),
                    EMBI = as.factor(EMBI), 
                    ISO = as.factor(ISO), 
                    POPULATION = as.numeric(POPULATION),
                    SITE_UPDATE = as.Date(SITE_UPDATE, origin = "1899-12-30"),
                    RESPONSE_UPDATE = as.Date(RESPONSE_UPDATE, origin = "1899-12-30"),
                    CASES = as.numeric(CASES), 
                    MORTALITIES = as.numeric(MORTALITIES), 
                    BORDER_CLOSURES_TRAVEL_RESTRICTIONS = as.factor(BORDER_CLOSURES_TRAVEL_RESTRICTIONS),
                    # CLOSURE_DATE = as.Date(CLOSURE_DATE, origin = "1899-12-30"),
                    QUARANTINE = as.factor(QUARANTINE),
                    # QUARANTINE_DATE = as.Date(QUARANTINE_DATE, origin = "1899-12-30"),
                    # OPENING_DATE = as.Date(OPENING_DATE, origin = "1899-12-30"),
                    FISCAL_STIMULUS_PERCENT = as.numeric(FISCAL_STIMULUS_PERCENT), 
                    ON_BUDGET = as.numeric(ON_BUDGET), 
                    OFF_BUDGET = as.numeric(OFF_BUDGET),
                    POLICY_RATE_OLD = as.numeric(POLICY_RATE_OLD), 
                    POLICY_RATE_NEW = as.numeric(POLICY_RATE_NEW),
                    POLICY_RATE_CHANGE = as.numeric(POLICY_RATE_CHANGE),
                    REPO_RATE_OLD = as.numeric(REPO_RATE_OLD), 
                    REPO_RATE_NEW = as.numeric(REPO_RATE_NEW),
                    REPO_CHANGE = as.numeric(REPO_CHANGE),
                    REVERSE_REPO_OLD = as.numeric(REVERSE_REPO_OLD), 
                    REVERSE_REPO_NEW = as.numeric(REVERSE_REPO_NEW), 
                    REVERSE_REPO_CHANGE = as.numeric(REVERSE_REPO_CHANGE), 
                    LIQUIDITY_TO_BUSINESSES = as.factor(LIQUIDITY_TO_BUSINESSES),
                    CREDIT_GUARANTEES = as.factor(CREDIT_GUARANTEES),
                    LOWER_RESERVE_REQUIREMENTS = as.factor(LOWER_RESERVE_REQUIREMENTS),
                    MACROPRUDENTIAL_EASING = as.factor(MACROPRUDENTIAL_EASING),
                    DEPOSIT_GUARANTEES = as.factor(DEPOSIT_GUARANTEES),
                    QE = as.factor(QE),
                    FED_SWAP_LINE = as.factor(FED_SWAP_LINE),
                    SWAP_LINE_AMOUNT = as.numeric(SWAP_LINE_AMOUNT), 
                    CFM = as.factor(CFM),
                    # CFM_Start = as.Date(CFM_Start, origin = "1899-12-30"),
                    FX_INTERVENTIONS = as.factor(FX_INTERVENTIONS), 
                    IMF_RCF = as.factor(IMF_RCF), 
                    RCF_AMOUNT_SDR = as.numeric(RCF_AMOUNT_SDR), 
                    RCF_AMOUNT_USD = as.numeric(RCF_AMOUNT_USD), 
                    # RCF_DATE = as.Date(RCF_DATE, origin = "1899-12-30"),
                    IMF_RFI = as.factor(IMF_RFI),
                    RFI_AMOUNT_SDR = as.numeric(RFI_AMOUNT_SDR), 
                    RFI_AMOUNT_USD = as.numeric(RFI_AMOUNT_USD),
                    # RFI_Date = as.Date(RFI_Date, origin = "1899-12-30"),
                    CURRENCY = as.factor(CURRENCY),
                    CURRENCY_CODE = as.factor(CURRENCY_CODE),
                    FX_AVG_2020_JAN_APR = as.numeric(FX_AVG_2020_JAN_APR),
                    FX_AVG_2020_Q1 = as.numeric(FX_AVG_2020_Q1),
                    FX_AVG_2019_SEP_DEC = as.numeric(FX_AVG_2019_SEP_DEC),
                    FX_AVG_2019_Q4 = as.numeric(FX_AVG_2019_Q4), 
                    FX_2019_12_31 = as.numeric(FX_2019_12_31),
                    FX_2020_03_31 = as.numeric(FX_2020_03_31),
                    FX_2020_04_30 = as.numeric(FX_2020_04_30),
                    FX_CHANGE_2020_Q1 = as.numeric(FX_CHANGE_2020_Q1),
                    FX_CHANGE_2020_JAN_APR = as.numeric(FX_CHANGE_2020_JAN_APR),
                    AVG_OIL_PRICE_SEP19 = as.numeric(AVG_OIL_PRICE_SEP19),
                    AVG_OIL_PRICE_OCT19 = as.numeric(AVG_OIL_PRICE_OCT19),
                    AVG_OIL_PRICE_NOV19 = as.numeric(AVG_OIL_PRICE_NOV19),
                    AVG_OIL_PRICE_DEC19 = as.numeric(AVG_OIL_PRICE_DEC19),
                    AVG_OIL_PRICE_JAN20 = as.numeric(AVG_OIL_PRICE_JAN20),
                    AVG_OIL_PRICE_FEB20 = as.numeric(AVG_OIL_PRICE_FEB20),
                    AVG_OIL_PRICE_MAR20 = as.numeric(AVG_OIL_PRICE_MAR20),
                    AVG_OIL_PRICE_APR20 = as.numeric(AVG_OIL_PRICE_APR20),
                    OIL_EXP_SEP19 = as.numeric(OIL_EXP_SEP19),
                    OIL_EXP_OCT19 = as.numeric(OIL_EXP_OCT19),
                    OIL_EXP_NOV19 = as.numeric(OIL_EXP_NOV19),
                    OIL_EXP_DEC19 = as.numeric(OIL_EXP_DEC19), 
                    OIL_EXP_JAN20 = as.numeric(OIL_EXP_JAN20), 
                    OIL_EXP_FEB20 = as.numeric(OIL_EXP_FEB20), 
                    OIL_REVENUE_SEP19 = as.numeric(OIL_REVENUE_SEP19),
                    OIL_REVENUE_OCT19 = as.numeric(OIL_REVENUE_OCT19),
                    OIL_REVENUE_NOV19 = as.numeric(OIL_REVENUE_NOV19),
                    OIL_REVENUE_DEC19 = as.numeric(OIL_REVENUE_DEC19),
                    OIL_REVENUE_JAN20 = as.numeric(OIL_REVENUE_JAN20),
                    OIL_REVENUE_FEB20 = as.numeric(OIL_REVENUE_FEB20),
                    OIL_REVENUE_2019Q4 = as.numeric(OIL_REVENUE_2019Q4),
                    OIL_REVENUE_2020Q1 = as.numeric(OIL_REVENUE_2020Q1),
                    OIL_REVENUE_CHANGE_Q42019_Q12020 = as.numeric(OIL_REVENUE_CHANGE_Q42019_Q12020),
                    YIELD_AVG_2020_JAN_APR = as.numeric(YIELD_AVG_2020_JAN_APR),
                    YIELD_AVG_2020_Q1 = as.numeric(YIELD_AVG_2020_Q1),
                    YIELD_AVG_2019_SEP_DEC = as.numeric(YIELD_AVG_2019_SEP_DEC), 
                    YIELD_AVG_2019_Q4 = as.numeric(YIELD_AVG_2019_Q4), 
                    YIELD_2019_12_31 = as.numeric(YIELD_2019_12_31),
                    YIELD_2020_03_31 = as.numeric(YIELD_2020_03_31),
                    YIELD_2020_04_30 = as.numeric(YIELD_2020_04_30),
                    YIELD_CHANGE_2020_Q1 = as.numeric(YIELD_CHANGE_2020_Q1),
                    YIELD_CHANGE_2020_JAN_APR = as.numeric(YIELD_CHANGE_2020_JAN_APR),
                    SPREAD_AVG_2020_JAN_APR = as.numeric(SPREAD_AVG_2020_JAN_APR),
                    SPREAD_AVG_2020_Q1 = as.numeric(SPREAD_AVG_2020_Q1),
                    SPREAD_AVG_2019_SEP_DEC = as.numeric(SPREAD_AVG_2019_SEP_DEC),
                    SPREAD_AVG_2019_Q4 = as.numeric(SPREAD_AVG_2019_Q4),
                    SPREAD_2019_12_31 = as.numeric(SPREAD_2019_12_31),
                    SPREAD_2020_03_31 = as.numeric(SPREAD_2020_03_31),
                    SPREAD_2020_04_30 = as.numeric(SPREAD_2020_04_30),
                    SPREAD_CHANGE_2020_Q1 = as.numeric(SPREAD_CHANGE_2020_Q1),
                    SPREAD_CHANGE_2020_JAN_APR = as.numeric(SPREAD_CHANGE_2020_JAN_APR),
                    DEBT_VS_GDP_2018 = as.numeric(DEBT_VS_GDP_2018),
                    CURRENT_ACCOUNT_VS_GDP_2019 = as.numeric(CURRENT_ACCOUNT_VS_GDP_2019),
                    PUBLIC_DEBT_VS_TAX_2019 = as.numeric(PUBLIC_DEBT_VS_TAX_2019),
                    EXTERNAL_DEBT_VS_GDP_2019 = as.numeric(EXTERNAL_DEBT_VS_GDP_2019),
                    FIVE_YEAR_SOVEREIGN_CDS_SPREADS_2019 = as.numeric(FIVE_YEAR_SOVEREIGN_CDS_SPREADS_2019),
                    S_T_EXTERNAL_DEBT_VS_RESERVES_2019 = as.numeric(S_T_EXTERNAL_DEBT_VS_RESERVES_2019),
                    RESERVE_VS_IMPORT_MONTHS_2019 = as.numeric(RESERVE_VS_IMPORT_MONTHS_2019),
                    RESERVES_VS_STD_2019 = as.numeric(RESERVES_VS_STD_2019),
                    CDS_1YR_NAME = as.character(CDS_1YR_NAME),
                    CDS_1YR_CURRENCY = as.factor(CDS_1YR_CURRENCY),
                    CDS_1YR_AVG_2020_JAN_APR = as.numeric(CDS_1YR_AVG_2020_JAN_APR),
                    CDS_1YR_AVG_2020_Q1 = as.numeric(CDS_1YR_AVG_2020_Q1), 
                    CDS_1YR_AVG_2019_SEP_DEC = as.numeric(CDS_1YR_AVG_2019_SEP_DEC),
                    CDS_1YR_AVG_2019_Q4 = as.numeric(CDS_1YR_AVG_2019_Q4),
                    CDS_1YR_2019_12_31 = as.numeric(CDS_1YR_2019_12_31), 
                    CDS_1YR_2020_03_31 = as.numeric(CDS_1YR_2020_03_31), 
                    CDS_1YR_2020_04_30 = as.numeric(CDS_1YR_2020_04_30), 
                    CDS_1YR_CHANGE_2020_Q1 = as.numeric(CDS_1YR_CHANGE_2020_Q1), 
                    CDS_1YR_CHANGE_2020_JAN_APR = as.numeric(CDS_1YR_CHANGE_2020_JAN_APR),
                    CDS_5YR_NAME = as.character(CDS_5YR_NAME), 
                    CDS_5YR_CURRENCY = as.factor(CDS_5YR_CURRENCY), 
                    CDS_5YR_AVG_2020_JAN_APR = as.numeric(CDS_5YR_AVG_2020_JAN_APR), 
                    CDS_5YR_AVG_2020_Q1 = as.numeric(CDS_5YR_AVG_2020_Q1), 
                    CDS_5YR_AVG_2019_SEP_DEC = as.numeric(CDS_5YR_AVG_2019_SEP_DEC), 
                    CDS_5YR_AVG_2019_Q4 = as.numeric(CDS_5YR_AVG_2019_Q4), 
                    CDS_5YR_2019_12_31 = as.numeric(CDS_5YR_2019_12_31), 
                    CDS_5YR_2020_03_31 = as.numeric(CDS_5YR_2020_03_31), 
                    CDS_5YR_2020_04_30 = as.numeric(CDS_5YR_2020_04_30), 
                    CDS_5YR_CHANGE_PERCENT_2020_Q1 = as.numeric(CDS_5YR_CHANGE_PERCENT_2020_Q1), 
                    CDS_5YR_CHANGE_2020_Q1 = as.numeric(CDS_5YR_CHANGE_2020_Q1), 
                    CDS_5YR_CHANGE_2020_JAN_APR = as.numeric(CDS_5YR_CHANGE_2020_JAN_APR), 
                    SWF_VOLUME = as.numeric(SWF_VOLUME) )

regressionDF <- rawDataDF

#Plausibility check for formatting of variables
glimpse(rawDataDF)
View(rawDataDF)
nrow(rawDataDF)
ncol(rawDataDF)
glimpse(rawDataDF)







######################
####Analysis of Q1
######################

##FX rate

#selecting variables to inspect change in fx rate over Q1
CurrencyDF <- select(rawDataDF, COUNTRY, FX_CHANGE_2020_Q1)

#Identifying the country with the mildest and harshest depreciation in Q1
countryWithMinDepreciationQ1 <- filter(CurrencyDF,  FX_CHANGE_2020_Q1 == min(CurrencyDF$FX_CHANGE_2020_Q1) )
countryWithMinDepreciationQ1
countryWithMaxDepreciationQ1 <- filter(CurrencyDF,  FX_CHANGE_2020_Q1 == max(CurrencyDF$FX_CHANGE_2020_Q1) )
countryWithMaxDepreciationQ1
sortedCurrencyDF <- arrange(CurrencyDF, FX_CHANGE_2020_Q1)

#Sorting the countries accordig to depreciation strength
sortedCurrencyDF <- arrange(CurrencyDF, FX_CHANGE_2020_Q1)

#Looking at the sorted data
View(sortedCurrencyDF)

head(sortedCurrencyDF, n= 5)
tail(sortedCurrencyDF, n= 5)

#Creating barplot to compare fx change in Q1
sortedCurrencyDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(FX_CHANGE_2020_Q1))) %>%
  ggplot( aes(COUNTRY , FX_CHANGE_2020_Q1, label = FX_CHANGE_2020_Q1)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("%") + 
  guides(fill=FALSE) +
  ggtitle("FX change over Q1") + 
  theme_minimal() 

#saving graph
ggsave("FX_CHANGE_Q1.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02" )

#Interpretation
#It turns out that Egypt experienced a slight appreciation whereas Brazil experienced an almost 30% depreciation
#There's no clear picture if oil exporters lost the most. 
#On the one hand, Egypt, Azerbaijan, Oman and UAE all appreciated
#On the other hand, Nigera, Colombia, Mexico, and the Russian Federation, South Africa and Brazil all depreciated.
#While not all of them are oil exporters, they are commodity exporters, so there really isn't a clear picture.







##Q1 spread

#selecting variables to inspect spread development
SpreadDF <- select(rawDataDF, COUNTRY,	SPREAD_CHANGE_2020_Q1,	SPREAD_CHANGE_2020_JAN_APR)

#count number of observations for which observations on SPREAD_CHANGE_2020_Q1 is NA
sum(SpreadDF$SPREAD_CHANGE_2020_Q1 == "NA")

#Clean NA from SPREAD_CHANGE_2020_Q1
cleanSpreadDF <- filter(SpreadDF, SPREAD_CHANGE_2020_Q1 != "NA")
cleanSpreadDF <- mutate(cleanSpreadDF, SPREAD_CHANGE_2020_Q1 = as.numeric(cleanSpreadDF$SPREAD_CHANGE_2020_Q1))

#Identifying the country with the minimum and maximum increase in Spread
countryWithMinSpreadIncreaseQ1 <- filter(cleanSpreadDF,  SPREAD_CHANGE_2020_Q1 == min(cleanSpreadDF$SPREAD_CHANGE_2020_Q1) )
countryWithMinSpreadIncreaseQ1
countryWithMaxSpreadIncreaseQ1 <- filter(cleanSpreadDF,  SPREAD_CHANGE_2020_Q1 == max(cleanSpreadDF$SPREAD_CHANGE_2020_Q1) )
countryWithMaxSpreadIncreaseQ1
sortedSpreadDF <- arrange(cleanSpreadDF, SPREAD_CHANGE_2020_Q1)
View(sortedSpreadDF)

#Sorting the countries
sortedSpreadDF <- arrange(sortedSpreadDF, SPREAD_CHANGE_2020_Q1)
View(sortedSpreadDF)
head(sortedSpreadDF)
tail(sortedSpreadDF)

#Creating barplot
sortedSpreadDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(SPREAD_CHANGE_2020_Q1))) %>%
  ggplot( aes(COUNTRY , SPREAD_CHANGE_2020_Q1)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("Percentage point") + 
  guides(fill=FALSE) +
  ggtitle("Spread change over Q1") + 
  theme_minimal()

ggsave("SPREAD_CHANGE_2020_Q1.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02" )







##Q1 CDS 1 year

#selecting variables to inspect spread development
CdsDF <- select(rawDataDF, COUNTRY,	CDS_1YR_CHANGE_2020_Q1, CDS_1YR_CHANGE_2020_JAN_APR)

#count number of observations for which observations on CDS_1YR_CHANGE_2020_Q1 is NA
sum(CdsDF$CDS_1YR_CHANGE_2020_Q1 == "NA")

#Clean NA from SPREAD_CHANGE_2020_Q1
cleanCdsDF <- filter(CdsDF, CDS_1YR_CHANGE_2020_Q1 != "NA")

#Identifying the country with the minimum and maximum increase in CDS
countryWithMinCDSIncreaseQ1 <- filter(cleanCdsDF,  CDS_1YR_CHANGE_2020_Q1 == min(cleanCdsDF$CDS_1YR_CHANGE_2020_Q1) )
countryWithMinCDSIncreaseQ1
countryWithMaxCDSIncreaseQ1 <- filter(cleanCdsDF,  CDS_1YR_CHANGE_2020_Q1 == max(cleanCdsDF$CDS_1YR_CHANGE_2020_Q1) )
countryWithMaxCDSIncreaseQ1
sortedCdsDF <- arrange(cleanCdsDF, CDS_1YR_CHANGE_2020_Q1)
View(sortedCdsDF)

#Creating barplot to compare CDS change
sortedCdsDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(CDS_1YR_CHANGE_2020_Q1))) %>%
  ggplot( aes(COUNTRY , CDS_1YR_CHANGE_2020_Q1)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("Decimals: 1 = 100%, 20 = 2000%") + 
  guides(fill=FALSE) +
  ggtitle("1 year CDS change over Q1") + 
  theme_minimal()

ggsave("CDS_CHANGE_2020_Q1.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02" )

######################
####Analysis of Q1 END
######################




######################
####Analysis of JAN-APR
######################

##Jan-Apr spread

#selecting variables to inspect spread development
SpreadDF <- select(rawDataDF, COUNTRY,	SPREAD_CHANGE_2020_Q1,	SPREAD_CHANGE_2020_JAN_APR)

#count number of observations for which observations on SPREAD_CHANGE_2020_JAN_APR is NA
sum(SpreadDF$SPREAD_CHANGE_2020_JAN_APR == "NA")

#Clean NA 
cleanSpreadDF <- filter(SpreadDF, SPREAD_CHANGE_2020_JAN_APR != "NA")
cleanSpreadDF <- mutate(cleanSpreadDF, SPREAD_CHANGE_2020_JAN_APR = as.numeric(cleanSpreadDF$SPREAD_CHANGE_2020_JAN_APR))

#Identifying the country with the minimum and maximum increase in Spread
countryWithMinSpreadIncrease <- filter(cleanSpreadDF,  SPREAD_CHANGE_2020_JAN_APR == min(cleanSpreadDF$SPREAD_CHANGE_2020_JAN_APR) )
countryWithMinSpreadIncrease
countryWithMaxSpreadIncrease <- filter(cleanSpreadDF,  SPREAD_CHANGE_2020_JAN_APR == max(cleanSpreadDF$SPREAD_CHANGE_2020_JAN_APR) )
countryWithMaxSpreadIncrease
sortedSpreadDF <- arrange(cleanSpreadDF, SPREAD_CHANGE_2020_JAN_APR)
View(sortedSpreadDF)

#Sorting the countries
sortedSpreadDF <- arrange(sortedSpreadDF, SPREAD_CHANGE_2020_JAN_APR)
View(sortedSpreadDF)
head(sortedSpreadDF)
tail(sortedSpreadDF)

#Creating barplot
sortedSpreadDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(SPREAD_CHANGE_2020_JAN_APR))) %>%
  ggplot( aes(COUNTRY , SPREAD_CHANGE_2020_JAN_APR)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("Percentage point") + 
  guides(fill=FALSE) +
  ggtitle("Spread change over Jan-Apr") + 
  theme_minimal()

ggsave("SPREAD_CHANGE_2020_JAN_APR.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02" )







##Jan-Apr CDS 1 year

#selecting variables to inspect spread development
CdsDF <- select(rawDataDF, COUNTRY,	CDS_1YR_CHANGE_2020_Q1, CDS_1YR_CHANGE_2020_JAN_APR)

#count number of observations for which observations on CDS_1YR_CHANGE_2020_JAN_APR is NA
sum(CdsDF$CDS_1YR_CHANGE_2020_JAN_APR == "NA")

#Clean NA from CDS_1YR_CHANGE_2020_JAN_APR
cleanCdsDF <- filter(CdsDF, CDS_1YR_CHANGE_2020_JAN_APR != "NA")

#Identifying the country with the minimum and maximum increase in CDS
countryWithMinCDSIncrease <- filter(cleanCdsDF,  CDS_1YR_CHANGE_2020_JAN_APR == min(cleanCdsDF$CDS_1YR_CHANGE_2020_JAN_APR) )
countryWithMinCDSIncrease
countryWithMaxCDSIncrease <- filter(cleanCdsDF,  CDS_1YR_CHANGE_2020_JAN_APR == max(cleanCdsDF$CDS_1YR_CHANGE_2020_JAN_APR) )
countryWithMaxCDSIncrease
sortedCdsDF <- arrange(cleanCdsDF, CDS_1YR_CHANGE_2020_JAN_APR)
View(sortedCdsDF)

#Creating barplot to compare CDS change
sortedCdsDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(CDS_1YR_CHANGE_2020_JAN_APR))) %>%
  ggplot( aes(COUNTRY , CDS_1YR_CHANGE_2020_JAN_APR)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("Decimals: 1 = 100%, 20 = 2000%") + 
  guides(fill=FALSE) +
  ggtitle("1 year CDS value change over Jan-Apr") + 
  theme_minimal()

ggsave("CDS_CHANGE_2020_JAN_APR.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02" )




##Jan-Apr FX rate

#selecting variables to inspect change in fx rate over Jan-Apr
CurrencyDF <- select(rawDataDF, COUNTRY, FX_CHANGE_2020_JAN_APR)

#Identifying the country with the mildest and harshest depreciation in Jan-Apr
countryWithMinDepreciation <- filter(CurrencyDF,  FX_CHANGE_2020_JAN_APR == min(CurrencyDF$FX_CHANGE_2020_JAN_APR) )
countryWithMinDepreciation
countryWithMaxDepreciation <- filter(CurrencyDF,  FX_CHANGE_2020_JAN_APR == max(CurrencyDF$FX_CHANGE_2020_JAN_APR) )
countryWithMaxDepreciation
sortedCurrencyDF <- arrange(CurrencyDF, FX_CHANGE_2020_JAN_APR)

#Sorting the countries accordig to depreciation strength
sortedCurrencyDF <- arrange(CurrencyDF, FX_CHANGE_2020_JAN_APR)

#Looking at the sorted data
View(sortedCurrencyDF)

head(sortedCurrencyDF, n= 5)
tail(sortedCurrencyDF, n= 5)

#Creating barplot to compare fx change in Q1
sortedCurrencyDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(FX_CHANGE_2020_JAN_APR))) %>%
  ggplot( aes(COUNTRY , FX_CHANGE_2020_JAN_APR, label = FX_CHANGE_2020_JAN_APR)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("%") + 
  guides(fill=FALSE) +
  ggtitle("FX change over Jan-Apr") + 
  theme_minimal() 

#saving graph
ggsave("FX_CHANGE_JAN_APR.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02" )

#Interpretation
#It turns out that Egypt experienced a slight appreciation whereas Brazil experienced an almost 30% depreciation
#There's no clear picture if oil exporters lost the most. 
#On the one hand, Egypt, Azerbaijan, Oman and UAE all appreciated
#On the other hand, Nigera, Colombia, Mexico, and the Russian Federation, South Africa and Brazil all depreciated.
#While not all of them are oil exporters, they are commodity exporters, so there really isn't a clear picture.

######################
####Analysis of Jan-Apr END
######################























#Q1 Oil revenue

#selecting variables to inspect change in oil revenues
OilDF <- select(rawDataDF, COUNTRY, OIL_REVENUE_2019Q4, OIL_REVENUE_2020Q1)
OilDF

#calculating change in oil revenues form 2019Q4 to 2020Q1
OilDF <- mutate(OilDF, OIL_REVENUE_CHANGE = (OIL_REVENUE_2020Q1-OIL_REVENUE_2019Q4) )
OilDF

sum(is.na(OilDF$OIL_REVENUE_CHANGE) )

#Clean NA from SPREAD_CHANGE_2020_Q1
cleanOilDF <- filter(OilDF, is.na(OilDF$OIL_REVENUE_CHANGE)==FALSE )
cleanOilDF






######################################################
# Depreciation in Q1 vs depreciation in Jan-Apr, with Interventions
rawDataDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(FX_CHANGE_2020_Q1)), FX_INTERVENTIONS = as.factor(FX_INTERVENTIONS) ) %>%
  ggplot( aes(x = FX_CHANGE_2020_Q1, y = FX_CHANGE_2020_JAN_APR, label = COUNTRY, col = FX_INTERVENTIONS)) + 
  geom_point(stat="identity") + 
  xlab("FX_CHANGE_2020_Q1 in percent ") + # Set axis labels
  ylab("FX_CHANGE_2020_JAN_APR in percent") + 
  guides(fill=FALSE) +
  ggtitle("") + 
  theme_minimal() +
  theme(legend.position="top")
ggsave("FX_CHANGE_Q1_VS_JAN_APR_INTERVENTIONS.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")
######################################################


######################################################
# Depreciation in Q1 vs depreciation in Jan-Apr, with QE
rawDataDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(FX_CHANGE_2020_Q1)), FX_INTERVENTIONS = as.factor(FX_INTERVENTIONS) ) %>%
  ggplot( aes(x = FX_CHANGE_2020_Q1, y = FX_CHANGE_2020_JAN_APR, label = COUNTRY, col = QE)) + 
  geom_point(stat="identity") + 
  xlab("FX_CHANGE_2020_Q1 in percent") + # Set axis labels
  ylab("FX_CHANGE_2020_JAN_APR in percent") + 
  guides(fill=FALSE) +
  ggtitle("") + 
  theme_minimal() +
  theme(legend.position="top")
ggsave("FX_CHANGE_Q1_VS_JAN_APR_QE.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")
######################################################


######################################################
#Reserves vs depreciation
rawDataDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(FX_CHANGE_2020_JAN_APR)), FX_INTERVENTIONS = factor(FX_INTERVENTIONS) ) %>%
  ggplot( aes(x = RESERVE_VS_IMPORT_MONTHS_2019, y = FX_CHANGE_2020_JAN_APR, label = COUNTRY, col = FX_INTERVENTIONS)) + 
  geom_point(stat="identity") + 
  xlab("Reserves in months of imports") + # Set axis labels
  ylab("Depreciation Jan-Apr in percent") + 
  ggtitle("") + 
  theme_minimal() +
  theme(legend.position="top")
ggsave("RESERVES_VS_DEPRECIATION.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")
######################################################



######################################################
#Depreciation vs Interventions
rawDataDF %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(FX_CHANGE_2020_JAN_APR)), FX_INTERVENTIONS = factor(FX_INTERVENTIONS) ) %>%
  ggplot( aes(x = COUNTRY, y = FX_CHANGE_2020_JAN_APR, color = FX_INTERVENTIONS, fill = FX_INTERVENTIONS)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("Jan-Apr depreciation in percent") + 
  guides(fill=FALSE) +
  ggtitle("FX change Jan-Apr") + 
  theme_minimal() 
theme(legend.position="bottom")
ggsave("DEPRECIATION_VS_INTERVENTION.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")
######################################################


######################################################
#Depreciation vs Interventions in April
fxDF <- select(rawDataDF, COUNTRY, FX_INTERVENTIONS, FX_2020_03_31, FX_2020_04_30 )
fxDF <- mutate(fxDF, FX_CHANGE_APRIL = FX_2020_04_30-FX_2020_03_31) %>% mutate(FX_CHANGE_APRIL_PERCENT =FX_CHANGE_APRIL*100/FX_2020_03_31 )
fxDF
fxDF %>% arrange(FX_CHANGE_APRIL_PERCENT) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(FX_CHANGE_APRIL_PERCENT)), FX_INTERVENTIONS = factor(FX_INTERVENTIONS) ) %>%
  ggplot( aes(x = COUNTRY, y = FX_CHANGE_APRIL_PERCENT, color = FX_INTERVENTIONS, fill = FX_INTERVENTIONS)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90, aes(fill=FX_INTERVENTIONS )) + 
  xlab("") + # Set axis labels
  ylab("") + 
  guides(fill=FALSE) +
  ggtitle("FX_CHANGE_APRIL_PERCENT") + 
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("DEPRECIATION_VS_INTERVENTION_APRIL.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")
######################################################


######################################################
#Depreciation January-April

#Histogram with automatic bins
ggplot(rawDataDF, aes(FX_CHANGE_2020_JAN_APR)) + 
  geom_histogram() + 
  ggtitle("Depreciation over Jan-Apr, automatic binwidth") 
ggsave("Depreciation over Jan-Apr, automatic binwidth.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")

#Histogram with manual number of bins
ggplot(rawDataDF, aes(FX_CHANGE_2020_JAN_APR)) + 
  geom_histogram(bins=3) + 
  ggtitle("Depreciation over Jan-Apr, manual number of bins =3") 
ggsave("Depreciation over Jan-Apr, manual number of bins =3.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")

#Histogram with manual binwidth
ggplot(rawDataDF, aes(FX_CHANGE_2020_JAN_APR)) + 
  geom_histogram(binwidth=3) + 
  ggtitle("Depreciation over Jan-Apr, manual binwidth = 3") 
ggsave("Depreciation over Jan-Apr, manual binwidth =3.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")


#Histogram with manual breaks
bins <- c(0, 5, 15)
ggplot(rawDataDF, aes(FX_CHANGE_2020_JAN_APR)) + 
  geom_histogram(breaks = bins) + 
  ggtitle("Depreciation over Jan-Apr, breaks at 0, 5, 15") 
ggsave("Depreciation over Jan-Apr, breaks at 0, 5, 15.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")




ggsave("DEPRECIATION_VS_INTERVENTION_APRIL.pdf", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/02")
######################################################


















