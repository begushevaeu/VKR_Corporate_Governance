#### Загрузка пакетов ####

library(dplyr)
library(missForest)
library(scipub)

#### Загрузка данных ####

data_raw <- Датасет

#### Создание вспомогательных переменных ####

Hot <- c("2005", "2006", "2007", "2010", "2011", "2012", "2020", "2021", "2023", "2024")
Tech <- c("TMT", "Biotech", "Computers & Electronics", "Telecommunications")

#### === Обработка под первый день === ####

data_day1 <- data_raw

data_day1 <- dplyr::select(data_day1, "Underpricing_1",
                      
                      "Mean_age", "Board_size", "Gender_variability",
                      
                      "Independence", "Independent_leader",
                      
                      "Current_exp", "Past_exp", 
                      
                      "Number_of_committees", 
                      
                      "Foreign_directors", 
                      
                      "Total_assets", "Age_of_company", "Year", "Industry", 
                      "ROA", "Net_Debt_EBITDA", "Current_ratio", "EBITDA_Margin") 

data_day1$Year_hot <- ifelse(data_day1$Year %in% Hot, 1, 0)
data_day1$Tech_sector <- ifelse(data_day1$Industry %in% Tech, 1, 0)
data_day1$Industry <- NULL
data_day1$Year <- NULL

#### === Обработка под тридцатый день === ####

data_day30 <- data_raw

data_day30 <- dplyr::select(data_day30, "Underpricing_30",
                           
                           "Mean_age", "Board_size", "Gender_variability",
                           
                           "Independence", "Independent_leader",
                           
                           "Current_exp", "Past_exp", 
                           
                           "Number_of_committees", 
                           
                           "Foreign_directors", 
                           
                           "Total_assets", "Age_of_company", "Year", "Industry", 
                           "ROA", "Net_Debt_EBITDA", "Current_ratio", "EBITDA_Margin") 

data_day30$Year_hot <- ifelse(data_day30$Year %in% Hot, 1, 0)
data_day30$Tech_sector <- ifelse(data_day30$Industry %in% Tech, 1, 0)
data_day30$Industry <- NULL
data_day30$Year <- NULL

#### Сохраняем промежуточные данные ####

saveRDS(data_day1, file = "data/day1_raw.rds")
saveRDS(data_day30, file = "data/day30_raw.rds")