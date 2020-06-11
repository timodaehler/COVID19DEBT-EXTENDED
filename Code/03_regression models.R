#Author: Timo Daehler, daehler@usc.edu
#Date of last update: 14 Mai 2020
#Purpose: First couple of regression to see if I can find a pattern in the winners and losers
#Inputs: Script 02. In particular the rawDataDF
#Outputs: 
#Other relevant notes: 



#########################################################
####Regression Analysis of Jan-Apr 
#########################################################


#Define the data from sheet 02. There I defined regression DF from rawDataDF. 
data <- regressionDF

#########################################################
#Dependent variable: SPREAD_CHANGE_2020_JAN_APR
#########################################################

model1 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + DEBT_VS_GDP_2018 , data = data )
summary(model1)

model2 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019, data = data )
summary(model2)

model3 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019 + SWF_VOLUME, data = data )
summary(model3)

model4 <- lm(SPREAD_CHANGE_2020_JAN_APR ~ -1 + FX_INTERVENTIONS * RESERVE_VS_IMPORT_MONTHS_2019 + FED_SWAP_LINE + PUBLIC_DEBT_VS_TAX_2019 + SWF_VOLUME + POLICY_RATE_CHANGE, data = data )
summary(model4)
?lm


#SPREAD_CHANGE_2020_JAN_APR as a function of
#pandemic threat (incidents per 100000) or better mortality rate
#current account balance * oil price effect
#

#########################################################
#Dependent variable: CDS_5YR_CHANGE_2020_JAN_APR
CDS_5YR_CHANGE_2020_JAN_APR
#########################################################
model1 <- lm(CDS_5YR_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + DEBT_VS_GDP_2018 , data = data )
summary(model1)

model2 <- lm(CDS_5YR_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019, data = data )
summary(model2)

model3 <- lm(CDS_5YR_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019 + SWF_VOLUME, data = data )
summary(model3)

model4 <- lm(CDS_5YR_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS * RESERVE_VS_IMPORT_MONTHS_2019 + FED_SWAP_LINE + PUBLIC_DEBT_VS_TAX_2019 + SWF_VOLUME + POLICY_RATE_CHANGE, data = data )
summary(model4)

#########################################################
#Dependent variable: FX_CHANGE_2020_JAN_APR
#########################################################
model1 <- lm(FX_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + DEBT_VS_GDP_2018 , data = data )
summary(model1)

model2 <- lm(FX_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019, data = data )
summary(model2)

model3 <- lm(FX_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS + PUBLIC_DEBT_VS_TAX_2019 + RESERVE_VS_IMPORT_MONTHS_2019 + SWF_VOLUME, data = data )
summary(model3)

model4 <- lm(FX_CHANGE_2020_JAN_APR ~ FX_INTERVENTIONS * RESERVE_VS_IMPORT_MONTHS_2019  + PUBLIC_DEBT_VS_TAX_2019 + SWF_VOLUME + POLICY_RATE_CHANGE, data = data )
summary(model4)


