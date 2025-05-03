library(dplyr)
library(MASS)
library(stargazer)

#### ПОРОГ 5% ####

#### === Загрузка данных === ####

data_BS <- readRDS("data/regressions/raw_log/day30/day30_Board_size_log_log5.rds")
data_In <- readRDS("data/regressions/raw_log/day30/day30_Independence_log5.rds")
data_MA <- readRDS("data/regressions/raw_log/day30/day30_Mean_age_log_log5.rds")
data_GV <- readRDS("data/regressions/raw_log/day30/day30_Gender_variability_log5.rds")
data_IL <- readRDS("data/regressions/raw_log/day30/day30_Independent_leader_log5.rds")
data_CE <- readRDS("data/regressions/raw_log/day30/day30_Current_exp_log_log5.rds")
data_PE <- readRDS("data/regressions/raw_log/day30/day30_Past_exp_log_log5.rds")
data_NC <- readRDS("data/regressions/raw_log/day30/day30_Number_of_committees_log5.rds")
data_FD <- readRDS("data/regressions/raw_log/day30/day30_Foreign_directors_log5.rds")

#### === ФИНАЛЬНЫЕ МОДЕЛИ === ####

## Board size
model_full_BS <- glm(UNDP_5 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_BS)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS <- glm(formula(model_step_BS), family = "binomial", data = data_BS)

## Independence
model_full_In <- glm(UNDP_5 ~ Independence + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_In)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence))
In <- glm(formula(model_step_In), family = "binomial", data = data_In)

## Mean age
model_full_MA <- glm(UNDP_5 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_MA)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA <- glm(formula(model_step_MA), family = "binomial", data = data_MA)

## Gender variability
model_full_GV <- glm(UNDP_5 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_GV)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV <- glm(formula(model_step_GV), family = "binomial", data = data_GV)

## Independent leader
model_full_IL <- glm(UNDP_5 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_IL)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL <- glm(formula(model_step_IL), family = "binomial", data = data_IL)

## Current exp
model_full_CE <- glm(UNDP_5 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_CE)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE <- glm(formula(model_step_CE), family = "binomial", data = data_CE)

## Past exp
model_full_PE <- glm(UNDP_5 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_PE)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE <- glm(formula(model_step_PE), family = "binomial", data = data_PE)

## Number of committees
model_full_NC <- glm(UNDP_5 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_NC)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC <- glm(formula(model_step_NC), family = "binomial", data = data_NC)

## Foreign directors
model_full_FD <- glm(UNDP_5 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_FD)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD <- glm(formula(model_step_FD), family = "binomial", data = data_FD)

#### === Выгружаем === ####

stargazer(BS, In, MA, GV, IL, CE, PE, NC, FD,
          type = "html",
          out = "output/logit_regressions_one_var/day30_raw_log5.html",
          title = "Логистические регрессии (5% порог) по грязным данным (day30)")

#### ПОРОГ 10% ####

#### === Загрузка данных === ####

data_BS <- readRDS("data/regressions/raw_log/day30/day30_Board_size_log_log10.rds")
data_In <- readRDS("data/regressions/raw_log/day30/day30_Independence_log10.rds")
data_MA <- readRDS("data/regressions/raw_log/day30/day30_Mean_age_log_log10.rds")
data_GV <- readRDS("data/regressions/raw_log/day30/day30_Gender_variability_log10.rds")
data_IL <- readRDS("data/regressions/raw_log/day30/day30_Independent_leader_log10.rds")
data_CE <- readRDS("data/regressions/raw_log/day30/day30_Current_exp_log_log10.rds")
data_PE <- readRDS("data/regressions/raw_log/day30/day30_Past_exp_log_log10.rds")
data_NC <- readRDS("data/regressions/raw_log/day30/day30_Number_of_committees_log10.rds")
data_FD <- readRDS("data/regressions/raw_log/day30/day30_Foreign_directors_log10.rds")

#### === ФИНАЛЬНЫЕ МОДЕЛИ === ####

## Board size
model_full_BS <- glm(UNDP_10 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_BS)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS <- glm(formula(model_step_BS), family = "binomial", data = data_BS)

## Independence
model_full_In <- glm(UNDP_10 ~ Independence + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_In)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence))
In <- glm(formula(model_step_In), family = "binomial", data = data_In)

## Mean age
model_full_MA <- glm(UNDP_10 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_MA)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA <- glm(formula(model_step_MA), family = "binomial", data = data_MA)

## Gender variability
model_full_GV <- glm(UNDP_10 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_GV)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV <- glm(formula(model_step_GV), family = "binomial", data = data_GV)

## Independent leader
model_full_IL <- glm(UNDP_10 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_IL)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL <- glm(formula(model_step_IL), family = "binomial", data = data_IL)

## Current exp
model_full_CE <- glm(UNDP_10 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_CE)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE <- glm(formula(model_step_CE), family = "binomial", data = data_CE)

## Past exp
model_full_PE <- glm(UNDP_10 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_PE)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE <- glm(formula(model_step_PE), family = "binomial", data = data_PE)

## Number of committees
model_full_NC <- glm(UNDP_10 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_NC)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC <- glm(formula(model_step_NC), family = "binomial", data = data_NC)

## Foreign directors
model_full_FD <- glm(UNDP_10 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_FD)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD <- glm(formula(model_step_FD), family = "binomial", data = data_FD)

#### === Выгружаем === ####

stargazer(BS, In, MA, GV, IL, CE, PE, NC, FD,
          type = "html",
          out = "output/logit_regressions_one_var/day30_raw_log10.html",
          title = "Логистические регрессии (10% порог) по грязным данным (day30)")
