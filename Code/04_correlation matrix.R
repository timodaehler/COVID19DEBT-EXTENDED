#Author: Timo Daehler, daehler@usc.edu
#Date of last update: 20 Mai 2020
#Purpose: Correlation matrix and first couple of regressions after I completed the dataset
#Inputs: 
#Outputs: Figures in folder in folder "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04"
#Other relevant notes: 


#loading required packages
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("PerformanceAnalytics")
#install.packages("GGally")
#install.packages("naniar")
#install.packages("ggcorrplot")
#install.packages("arm")
#install.packages("stargazer") 
#install.packages("stevemisc")
#install.packages("rticles")
library(stevemisc)
library(rticles)
library(ggplot2)
library(stargazer)
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
library(corrplot)
library(PerformanceAnalytics)
library(GGally)
library(naniar)
library(ggcorrplot)
#library(arm)

#importing the Data.xls sheet
rawDF <- read_excel("/Users/timodaehler/Desktop/COVID19DEBT/Data/Data.xlsx", sheet = 1)
#View(rawDF)

#rawDF <- rawDF %>% mutate_all(as.character)

#Converting all "NA" to empty values
rawDF[rawDF == "NA"] <- ""

#Viewing the data
View(rawDF)


#here I change the type of the variable so that numbers are numeric, strings are characters, etc. 
rawDF <- mutate(rawDF, 
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
                    INFECTIONS_2020_01_22 = as.numeric(INFECTIONS_2020_01_22),
                    INFECTIONS_2020_01_31 = as.numeric(INFECTIONS_2020_01_31),
                    INFECTIONS_2020_02_29 = as.numeric(INFECTIONS_2020_02_29),
                    INFECTIONS_2020_03_31 = as.numeric(INFECTIONS_2020_03_31),
                    INFECTIONS_2020_04_15 = as.numeric(INFECTIONS_2020_04_15),
                    INFECTIONS_2020_04_20 = as.numeric(INFECTIONS_2020_04_20),
                    INFECTION_RATE_2020_04_20 = as.numeric(INFECTION_RATE_2020_04_20),
                    INFECTIONS_2020_04_25 = as.numeric(INFECTIONS_2020_04_25),
                    INFECTIONS_2020_04_30 = as.numeric(INFECTIONS_2020_04_30),
                    DEATHS_2020_01_22 = as.numeric(DEATHS_2020_01_22),
                    DEATHS_2020_01_31 = as.numeric(DEATHS_2020_01_31),
                    DEATHS_2020_02_29 = as.numeric(DEATHS_2020_02_29),
                    DEATHS_2020_03_31 = as.numeric(DEATHS_2020_03_31),
                    DEATHS_2020_04_15 = as.numeric(DEATHS_2020_04_15),
                    DEATHS_2020_04_20 = as.numeric(DEATHS_2020_04_20),
                    DEATH_RATE_2020_04_20 = as.numeric(DEATH_RATE_2020_04_20),
                    DEATHS_2020_04_25 = as.numeric(DEATHS_2020_04_25),
                    DEATHS_2020_04_30 = as.numeric(DEATHS_2020_04_30),
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
                    TOTAL_EXPORT_2019	= as.numeric(TOTAL_EXPORT_2019),
                    OIL_EXPORT_2019	= as.numeric(OIL_EXPORT_2019),
                    OIL_EXPORT_SHARE_2019_DECIMAL	= as.numeric(OIL_EXPORT_SHARE_2019_DECIMAL),
                    OIL_EXPORT_SHARE_2019_PERCENT	= as.numeric(OIL_EXPORT_SHARE_2019_PERCENT),
                    TOTAL_IMPORT_2019	= as.numeric(TOTAL_IMPORT_2019),
                    OIL_IMPORT_2019	= as.numeric(OIL_IMPORT_2019),
                    OIL_IMPORT_SHARE_2019_DECIMAL	= as.numeric(OIL_IMPORT_SHARE_2019_DECIMAL),
                    OIL_IMPORT_SHARE_2019_PERCENT	= as.numeric(OIL_IMPORT_SHARE_2019_PERCENT),
                    TOTAL_EXPORT_2018	= as.numeric(TOTAL_EXPORT_2018),
                    OIL_EXPORT_2018	= as.numeric(OIL_EXPORT_2018),
                    OIL_EXPORT_SHARE_2018_DECIMAL	= as.numeric(OIL_EXPORT_SHARE_2018_DECIMAL),
                    OIL_EXPORT_SHARE_2018_PERCENT	= as.numeric(OIL_EXPORT_SHARE_2018_PERCENT),
                    TOTAL_IMPORT_2018	= as.numeric(TOTAL_IMPORT_2018),
                    OIL_IMPORT_2018	= as.numeric(OIL_IMPORT_2018),
                    OIL_IMPORT_SHARE_2018_DECIMAL	= as.numeric(OIL_IMPORT_SHARE_2018_DECIMAL),
                    OIL_IMPORT_SHARE_2018_PERCENT	= as.numeric(OIL_IMPORT_SHARE_2018_PERCENT),
                    OIL_PRICE_CHANGE_AVG_2019_VS_AVG_1Q2020_DECIMAL	= as.numeric(OIL_PRICE_CHANGE_AVG_2019_VS_AVG_1Q2020_DECIMAL),
                    OIL_PRICE_CHANGE_AVG_2019_VS_AVG_1Q2020_PERCENT	= as.numeric(OIL_PRICE_CHANGE_AVG_2019_VS_AVG_1Q2020_PERCENT),
                    OIL_PRICE_CHANGE_AVG_2018_VS_AVG_1Q2020_DECIMAL	= as.numeric(OIL_PRICE_CHANGE_AVG_2018_VS_AVG_1Q2020_DECIMAL),
                    OIL_PRICE_CHANGE_AVG_2018_VS_AVG_1Q2020_PERCENT	= as.numeric(OIL_PRICE_CHANGE_AVG_2018_VS_AVG_1Q2020_PERCENT),
                    EXPORT_VS_GDP_2017_PERCENT = as.numeric(EXPORT_VS_GDP_2017_PERCENT),
                    EXPORT_VS_GDP_2017_DECIMAL	= as.numeric(EXPORT_VS_GDP_2017_DECIMAL),
                    EXPORT_VS_GDP_2018_PERCENT	= as.numeric(EXPORT_VS_GDP_2018_PERCENT),
                    EXPORT_VS_GDP_2018_DECIMAL	= as.numeric(EXPORT_VS_GDP_2018_DECIMAL),
                    IMPORT_VS_GDP_2017_PERCENT	= as.numeric(IMPORT_VS_GDP_2017_PERCENT),
                    IMPORT_VS_GDP_2017_DECIMAL	= as.numeric(IMPORT_VS_GDP_2017_DECIMAL),
                    IMPORT_VS_GDP_2018_PERCENT	= as.numeric(IMPORT_VS_GDP_2018_PERCENT),
                    IMPORT_VS_GDP_2018_DECIMAL	= as.numeric(IMPORT_VS_GDP_2018_DECIMAL),
                    OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT	= as.numeric(OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT)*100,
                    OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_DECIMAL	= as.numeric(OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_DECIMAL),
                    OIL_PRICE_IMPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT	= as.numeric(OIL_PRICE_IMPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT)*100,
                    OIL_PRICE_IMPORT_EFFECT_VS_GDP_2018_VS_1Q2020_DECIMAL	= as.numeric(OIL_PRICE_IMPORT_EFFECT_VS_GDP_2018_VS_1Q2020_DECIMAL),
                    OIL_PRICE_TOTAL_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT	= as.numeric(OIL_PRICE_TOTAL_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT)*100,
                    OIL_PRICE_TOTAL_EFFECT_VS_GDP_2018_VS_1Q2020_DECIMAL = as.numeric(OIL_PRICE_TOTAL_EFFECT_VS_GDP_2018_VS_1Q2020_DECIMAL),
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
                    CURRENT_ACCOUNT_VS_GDP_AVG_2014_2018 = as.numeric(CURRENT_ACCOUNT_VS_GDP_AVG_2014_2018),
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
                    SWF_VOLUME = as.numeric(SWF_VOLUME)    )



##Let's start with the analysis
############################################################################################################################################
#Here I create a visual overview of which data is available and which one is not. For best view click on "zoom"
vis_miss(rawDF) + theme(text = element_text(size=5), axis.text.x = element_text(angle = 90))
#Upon inspection of the graph we can see that there is not a lot that we can can gain fro the visual representation
#of missing values. While we can observe that overall we have decent data availability, there are clearly many variables for whic
#data is relatively scarce, which might mean that we will have only data on few observations.

#I save this visual overview
ggsave("missingdata.png", width=20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################



##Dependent variables: options
############################################################################################################################################
#Creating a vector of all four potential dependent variables, from which we can later choose one for the analysis
possibleOutcomeVariables <- c("SPREAD_CHANGE_2020_JAN_APR", "YIELD_CHANGE_2020_JAN_APR", "CDS_1YR_CHANGE_2020_JAN_APR", "FX_CHANGE_2020_JAN_APR")

#Naming the outcome variable for the graph
Spread <- possibleOutcomeVariables[1]
Yiel <- possibleOutcomeVariables[2]
CDS <- possibleOutcomeVariables[3]
Depreciation <- possibleOutcomeVariables[4]
############################################################################################################################################



##Dependent variables: data availability
############################################################################################################################################
#Creating a sub dataframe of the possible dependent variables
checkAvailabilityOfDependentVariable <- select(rawDF, COUNTRY, possibleOutcomeVariables)

#Calling the summary command gives us an overview of the NAs in this dataframe. It shows that we miss 8 CDS datapoints and 7 data points for yields and spreads
summary(checkAvailabilityOfDependentVariable)

#Here I create a visual overview of which data is available and which one is not. For best view click on "zoom"
vis_miss(checkAvailabilityOfDependentVariable) + theme(text = element_text(size=7), axis.text.x = element_text(angle = 90))

#I save this visual overview
ggsave("missingdata_dependentvariable.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################





##Independent variables: options
############################################################################################################################################
#Creating a vector all potential Covid independent variables , from which we can later choose one for the analysis
possiblePredictorsCovid    <- c("BORDER_CLOSURES_TRAVEL_RESTRICTIONS", "QUARANTINE","INFECTION_RATE_2020_04_20", "DEATH_RATE_2020_04_20")

#Creating a vector all potential Fiscal independent variables , from which we can later choose one for the analysis
possiblePredictorsFiscal    <- c("FISCAL_STIMULUS_PERCENT", "DEBT_VS_GDP_2018", "PUBLIC_DEBT_VS_TAX_2019")

#Creating a vector all potential Monetary independent variables , from which we can later choose one for the analysis
possiblePredictorsMonetary  <- c("POLICY_RATE_CHANGE", "FED_SWAP_LINE", "SWAP_LINE_AMOUNT", "FX_INTERVENTIONS", "FX_CHANGE_2020_JAN_APR", "S_T_EXTERNAL_DEBT_VS_RESERVES_2019", "RESERVE_VS_IMPORT_MONTHS_2019", "SWF_VOLUME")

#Creating a vector all potential Oil independent variables , from which we can later choose one for the analysis
possiblePredictorsOil     <- c("OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT", "OIL_PRICE_IMPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT", "OIL_PRICE_TOTAL_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT")

#aggregating the options into one vector
possibleIndependentVariables <- c(possiblePredictorsCovid, possiblePredictorsFiscal, possiblePredictorsMonetary, possiblePredictorsOil)
############################################################################################################################################





##Independent variables: data availability
############################################################################################################################################
#Creating a sub dataframe of the possible predictors
checkAvailabilityOfPredictors <- select(rawDF, COUNTRY, possibleIndependentVariables)

#Calling the summary command gives us an overview of the NAs in this dataframe. It shows that we miss 8 CDS datapoints and 7 data points for yields and spreads
summary(checkAvailabilityOfPredictors)

#Here I create a visual overview of which data is available and which one is not. For best view click on "zoom"
vis_miss(checkAvailabilityOfPredictors) + theme(text = element_text(size=7), axis.text.x = element_text(angle = 90))

#I save this visual overview
ggsave("missingdata_predictors.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################









############################################################################################################################################
##Correlation matrices:spread
############################################################################################################################################
#Creating a dataframe for the first correlation matrix. spread vs independent variables
SpreadDF <- select(rawDF, SPREAD_CHANGE_2020_JAN_APR, possibleIndependentVariables)
numberOfObservationsBeforeNaRemoval <- nrow(SpreadDF)
cleanSpreadDF <- na.omit(SpreadDF)
cleanSpreadDF <- cleanSpreadDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanSpreadDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanSpreadDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "All predictors vs spread", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_spread_vs_all.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanSpreadDF, title = "All predictors vs spread",
        upper = list(continuous = wrap("cor", size = 3))) + theme(text = element_text(size=3))
ggsave("Corrmatrix_spread_vs_all.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")


#It turns out that all these contain a lot of variables and may not aid too much in terms of interpretation. 
#Hence I create smaller matrices with fewer variables
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for spread vs covid variables
SpreadDF <- select(rawDF, SPREAD_CHANGE_2020_JAN_APR, possiblePredictorsCovid)
numberOfObservationsBeforeNaRemoval <- nrow(SpreadDF)
cleanSpreadDF <- na.omit(SpreadDF)
cleanSpreadDF <- cleanSpreadDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanSpreadDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanSpreadDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Covid predictors vs spread", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_spread_vs_covid.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanSpreadDF, title = "Covid predictors vs spread",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_spread_vs_covid.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for spread vs fiscal variables
SpreadDF <- select(rawDF, SPREAD_CHANGE_2020_JAN_APR, possiblePredictorsFiscal)
numberOfObservationsBeforeNaRemoval <- nrow(SpreadDF)
cleanSpreadDF <- na.omit(SpreadDF)
cleanSpreadDF <- cleanSpreadDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanSpreadDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanSpreadDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Fiscal variables vs spread", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_spread_vs_fiscal.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanSpreadDF, title = "Fiscal predictors vs spread",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_spread_vs_fiscal.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for spread vs monetary variables
SpreadDF <- select(rawDF, SPREAD_CHANGE_2020_JAN_APR, possiblePredictorsMonetary)
numberOfObservationsBeforeNaRemoval <- nrow(SpreadDF)
cleanSpreadDF <- na.omit(SpreadDF)
cleanSpreadDF <- cleanSpreadDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanSpreadDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanSpreadDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Monetary predictors vs spread", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_spread_vs_monetary.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")


#Creating the intricate graph of correlation coefficients and scatterplots etc
ggpairs(cleanSpreadDF, title = "Monetary predictors vs spread",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_spread_vs_monetary.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for spread vs oil variables
SpreadDF <- select(rawDF, SPREAD_CHANGE_2020_JAN_APR, possiblePredictorsOil)
numberOfObservationsBeforeNaRemoval <- nrow(SpreadDF)
cleanSpreadDF <- na.omit(SpreadDF)
cleanSpreadDF <- cleanSpreadDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanSpreadDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanSpreadDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Oil predictors vs spread", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_spread_vs_oil.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")


#Creating the intricate graph of correlation coefficients and scatterplots etc
ggpairs(cleanSpreadDF, title = "Oil predictors vs spread",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_spread_vs_oil.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################










############################################################################################################################################
##Correlation matrices:yield
############################################################################################################################################
#Creating a dataframe for the first correlation matrix. yield vs independent variables
YieldDF <- select(rawDF, YIELD_CHANGE_2020_JAN_APR, possibleIndependentVariables)
numberOfObservationsBeforeNaRemoval <- nrow(YieldDF)
cleanYieldDF <- na.omit(YieldDF)
cleanYieldDF <- cleanYieldDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanYieldDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanYieldDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "All predictors vs yield", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_yield_vs_all.png", width=20, height =20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanYieldDF, title = "All predictors vs yield",
        upper = list(continuous = wrap("cor", size = 3))) + theme(text = element_text(size=3))
ggsave("Corrmatrix_yield_vs_all.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for yield vs covid variables
YieldDF <- select(rawDF, YIELD_CHANGE_2020_JAN_APR, possiblePredictorsCovid)
numberOfObservationsBeforeNaRemoval <- nrow(YieldDF)
cleanYieldDF <- na.omit(YieldDF)
cleanYieldDF <- cleanYieldDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanYieldDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanYieldDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Covid predictors vs yield", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_yield_vs_covid.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. 
ggpairs(cleanYieldDF, title = "Covid predictors vs yield",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_yield_vs_covid.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for yield vs fiscal variables
YieldDF <- select(rawDF, YIELD_CHANGE_2020_JAN_APR, possiblePredictorsFiscal)
numberOfObservationsBeforeNaRemoval <- nrow(YieldDF)
cleanYieldDF <- na.omit(YieldDF)
cleanYieldDF <- cleanYieldDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanYieldDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanYieldDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Fiscal predictors vs yield", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_yield_vs_fiscal.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanYieldDF, title = "Fiscal predictors vs yield",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_yield_vs_fiscal.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for yield vs moneatary variables
YieldDF <- select(rawDF, YIELD_CHANGE_2020_JAN_APR, possiblePredictorsMonetary)
numberOfObservationsBeforeNaRemoval <- nrow(YieldDF)
cleanYieldDF <- na.omit(YieldDF)
cleanYieldDF <- cleanYieldDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanYieldDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanYieldDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Monetary predictors vs yield", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_yield_vs_monetary.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. 
ggpairs(cleanYieldDF, title = "Monetary predictors vs yield",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_yield_vs_monetary.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for yield vs oil variables
YieldDF <- select(rawDF, YIELD_CHANGE_2020_JAN_APR, possiblePredictorsOil)
numberOfObservationsBeforeNaRemoval <- nrow(YieldDF)
cleanYieldDF <- na.omit(YieldDF)
cleanYieldDF <- cleanYieldDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanYieldDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanYieldDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Oil predictors vs yield", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_yield_vs_oil.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. 
ggpairs(cleanYieldDF, title = "Oil predictors vs yield",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_yield_vs_oil.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################
















############################################################################################################################################
##Correlation matrices:CDS
############################################################################################################################################
#Creating a dataframe for the first correlation matrix. CDS vs independent variables
CDSDF <- select(rawDF, CDS_1YR_CHANGE_2020_JAN_APR, possibleIndependentVariables)
numberOfObservationsBeforeNaRemoval <- nrow(CDSDF)
cleanCDSDF <- na.omit(CDSDF)
cleanCDSDF <- cleanCDSDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanCDSDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanCDSDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "All predictors vs CDS", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_cds_vs_all.png", width=20, height =20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanCDSDF, title = "All predictors vs CDS",
        upper = list(continuous = wrap("cor", size = 3))) + theme(text = element_text(size=3))
ggsave("Corrmatrix_cds_vs_all.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for cds vs covid variables
CDSDF <- select(rawDF, CDS_1YR_CHANGE_2020_JAN_APR, possiblePredictorsCovid)
numberOfObservationsBeforeNaRemoval <- nrow(CDSDF)
cleanCDSDF <- na.omit(CDSDF)
cleanCDSDF <- cleanCDSDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanCDSDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanCDSDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Covid predictors vs CDS", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_cds_vs_covid.png",  path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc.
ggpairs(cleanCDSDF, title = "Covid predictors vs CDS",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_cds_vs_covid.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for cds vs fiscal variables
CDSDF <- select(rawDF, CDS_1YR_CHANGE_2020_JAN_APR, possiblePredictorsFiscal)
numberOfObservationsBeforeNaRemoval <- nrow(CDSDF)
cleanCDSDF <- na.omit(CDSDF)
cleanCDSDF <- cleanCDSDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanCDSDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanCDSDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Fiscal predictors vs CDS", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_cds_vs_fiscal.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc.
ggpairs(cleanCDSDF, title = "Fiscal predictors vs CDS",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_cds_vs_fiscal.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for cds vs moneatary variables
CDSDF <- select(rawDF, CDS_1YR_CHANGE_2020_JAN_APR, possiblePredictorsMonetary)
numberOfObservationsBeforeNaRemoval <- nrow(CDSDF)
cleanCDSDF <- na.omit(CDSDF)
cleanCDSDF <- cleanCDSDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanCDSDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanCDSDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Monetary predictors vs CDS", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_cds_vs_monetary.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc.
ggpairs(cleanCDSDF, title = "Monetary predictors vs CDS",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_cds_vs_monetary.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for cds vs oil variables
CDSDF <- select(rawDF, CDS_1YR_CHANGE_2020_JAN_APR, possiblePredictorsOil)
numberOfObservationsBeforeNaRemoval <- nrow(CDSDF)
cleanCDSDF <- na.omit(CDSDF)
cleanCDSDF <- cleanCDSDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanCDSDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanCDSDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Oil predictors vs CDS", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_cds_vs_oil.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc.
ggpairs(cleanCDSDF, title = "Oil predictors vs CDS",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_cds_vs_oil.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################









############################################################################################################################################
##Correlation matrices:FX change
############################################################################################################################################
#Creating a dataframe for the first correlation matrix. FX change vs independent variables
FXDF <- select(rawDF, FX_CHANGE_2020_JAN_APR, possibleIndependentVariables)
numberOfObservationsBeforeNaRemoval <- nrow(FXDF)
cleanFXDF <- na.omit(FXDF)
cleanFXDF <- cleanFXDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanFXDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanFXDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "All predictors vs FX change", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_fx_vs_all.png", width=20, height =20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanFXDF, title = "All predictors vs FX Change",
        upper = list(continuous = wrap("cor", size = 3))) + theme(text = element_text(size=3))
ggsave("Corrmatrix_fx_vs_all.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for fx change vs covid variables
FXDF <- select(rawDF, FX_CHANGE_2020_JAN_APR, possiblePredictorsCovid)
numberOfObservationsBeforeNaRemoval <- nrow(FXDF)
cleanFXDF <- na.omit(FXDF)
cleanFXDF <- cleanFXDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanFXDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanFXDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Covid predictors vs FX change", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_fx_vs_covid.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc. HARD TO READ
ggpairs(cleanFXDF, title = "Covid predictors vs FX Change",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_fx_vs_covid.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for fx change vs fiscal variables
FXDF <- select(rawDF, FX_CHANGE_2020_JAN_APR, possiblePredictorsFiscal)
numberOfObservationsBeforeNaRemoval <- nrow(FXDF)
cleanFXDF <- na.omit(FXDF)
cleanFXDF <- cleanFXDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanFXDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanFXDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Fiscal vs FX chage", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_fx_vs_fiscal.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc.
ggpairs(cleanFXDF, title = "Fiscal vs FX change",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_fx_vs_fiscal.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for fx change vs monetary variables
FXDF <- select(rawDF, FX_CHANGE_2020_JAN_APR, possiblePredictorsMonetary)
numberOfObservationsBeforeNaRemoval <- nrow(FXDF)
cleanFXDF <- na.omit(FXDF)
cleanFXDF <- cleanFXDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanFXDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanFXDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Monetary predictors vs FX change", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_fx_vs_monetary.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc.
ggpairs(cleanFXDF, title = "Monetary predictors vs FX change",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_fx_vs_monetary.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################

############################################################################################################################################
#Creating a dataframe for fx change vs oil variables
FXDF <- select(rawDF, FX_CHANGE_2020_JAN_APR, possiblePredictorsOil)
numberOfObservationsBeforeNaRemoval <- nrow(FXDF)
cleanFXDF <- na.omit(FXDF)
cleanFXDF <- cleanFXDF %>% mutate_all(as.numeric)
numberOfObservationsAfterNaRemoval <- nrow(cleanFXDF)

#Creating the correlation data matrix that can then be plotted in the next step
corr <- round(cor(cleanFXDF), 1)

#Creating the plot based on the correlation data matrix
ggcorrplot(corr, title = "Oil predictors vs FX change", legend.title = "Correlation",  hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3, tl.cex=5, tl.srt=90) 
ggsave("Corrplot_fx_vs_oil.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

#Creating the intricate graph of correlation coefficients and scatterplots etc.
ggpairs(cleanFXDF, title = "Oil predictors vs FX change",
        upper = list(continuous = wrap("cor", size = 7))) + theme(text = element_text(size=10))
ggsave("Corrmatrix_fx_vs_oil.png", width = 20, height = 20, path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")
############################################################################################################################################









############################################################################################################################################
##Regression models: spread as dependent variable
############################################################################################################################################
data <- rawDF

#########################################################
#Dependent variable: SPREAD_CHANGE_2020_JAN_APR
#########################################################
#In these models I explain the change in the spread between 31.12.2019 and 30.4.2020 as a function of some
#variables at the end of 2018, the end of 2019, and some things that happened in between January and April

model1 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ INFECTION_RATE_2020_04_20 + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019*FX_INTERVENTIONS + OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT  , data = data )
summary(model1)

model2 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ INFECTION_RATE_2020_04_20 + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019*FX_INTERVENTIONS + OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT  -1, data = data )
summary(model2)

model3 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ DEATH_RATE_2020_04_20 + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019 + OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT  , data = data )
summary(model3)

model4 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ DEATH_RATE_2020_04_20 + DEBT_VS_GDP_2018  + RESERVE_VS_IMPORT_MONTHS_2019 + OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT  -1, data = data )
summary(model4)

model5 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ INFECTION_RATE_2020_04_20 + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019 + OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT  -1, data = data )
summary(model5)

model6 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ DEATH_RATE_2020_04_20 + DEBT_VS_GDP_2018  + RESERVE_VS_IMPORT_MONTHS_2019 + OIL_PRICE_TOTAL_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT  -1, data = data )
summary(model6)


short_names <- mutate(data, 
                      spread = SPREAD_CHANGE_2020_JAN_APR,
                      infection_rate =INFECTION_RATE_2020_04_20,
                      death_rate = DEATH_RATE_2020_04_20,
                      debt_tax_ratio = PUBLIC_DEBT_VS_TAX_2019,
                      debt_ratio = DEBT_VS_GDP_2018,
                      months_of_reserves = RESERVE_VS_IMPORT_MONTHS_2019,
                      oil_export_eff = OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT,
                      cu_5yr_avg = CURRENT_ACCOUNT_VS_GDP_AVG_2014_2018 )

model1 <- lm(spread ~ death_rate + debt_tax_ratio + months_of_reserves + oil_export_eff + cu_5yr_avg , data = short_names )
summary(model1)

model2 <- lm(spread ~ death_rate + debt_tax_ratio + months_of_reserves + oil_export_eff*cu_5yr_avg , data = short_names )
summary(model2)

model3 <- lm(spread ~ death_rate + debt_tax_ratio + months_of_reserves + oil_export_eff + cu_5yr_avg -1, data = short_names )
summary(model3)

model4 <- lm(spread ~ death_rate + debt_tax_ratio + months_of_reserves + oil_export_eff*cu_5yr_avg -1, data = short_names )
summary(model4)



stargazer(model1, model2, title="Results with intercept", align=TRUE)

stargazer(model3, model4, title="Results without intercept", align=TRUE)








# From here on I run some histograms and boxplots to check for outliers



ggplot(short_names, aes(x=spread)) + 
        geom_histogram(binwidth = 1) + labs(title="SPREAD_CHANGE_2020_JAN_APR")
ggsave("spread_hist.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(x=death_rate)) + 
        geom_histogram(binwidth = 1) + labs(title="DEATH_RATE_2020_04_20")
ggsave("death_rate_hist.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(x=debt_tax_ratio)) + 
        geom_histogram() + labs(title="PUBLIC_DEBT_VS_TAX_2019")
ggsave("debt_tax_ratio_hist.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(x=debt_ratio)) + 
        geom_histogram() + labs(title="PUBLIC_DEBT_VS_TAX_2019")
ggsave("debt_ratio_hist.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(x=months_of_reserves)) + 
        geom_histogram() + labs(title="RESERVE_VS_IMPORT_MONTHS_2019")
ggsave("months_of_reserves_hist.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(x=oil_export_eff)) + 
        geom_histogram() + labs(title="OIL_PRICE_EXPORT_EFFECT_VS_GDP_2018_VS_1Q2020_PERCENT")
ggsave("oil_export_eff_hist.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(x=cu_5yr_avg)) + 
        geom_histogram() + labs(title="CURRENT_ACCOUNT_VS_GDP_AVG_2014_2018")
ggsave("cu_5yr_avg_hist.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")








ggplot(short_names, aes(y=spread)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)
ggsave("spread_boxplot.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(y=death_rate)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)
ggsave("death_rate_boxplot.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(y=debt_tax_ratio)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)
ggsave("debt_tax_ratio_boxplot.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(y=debt_ratio)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)
ggsave("debt_ratio_boxplot.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(y=months_of_reserves)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)
ggsave("months_of_reserves_boxplot.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(y=oil_export_eff)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)
ggsave("oil_export_eff_boxplot.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")

ggplot(short_names, aes(y=cu_5yr_avg)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)
ggsave("cu_5yr_avg_boxplot.png", path = "/Users/timodaehler/Desktop/COVID19DEBT/Plots/04")





model5 <- lm(spread ~ death_rate + debt_ratio + months_of_reserves + oil_export_eff + cu_5yr_avg , data = short_names )
summary(model5)

model6 <- lm(spread ~ death_rate + debt_ratio + months_of_reserves + oil_export_eff*cu_5yr_avg , data = short_names )
summary(model6)

model7 <- lm(spread ~ death_rate + debt_ratio + months_of_reserves + oil_export_eff + cu_5yr_avg -1, data = short_names )
summary(model7)

model8 <- lm(spread ~ death_rate + debt_ratio + months_of_reserves + oil_export_eff*cu_5yr_avg -1, data = short_names )
summary(model8)



stargazer(model5, model6, title="Results with intercept when we use debt/GDP instead of debt/tax ratio", align=TRUE)

stargazer(model7, model8, title="Results without intercept when we use debt/GDP instead of debt/tax ratio", align=TRUE)


