library(dplyr)
library(MASS)
library(stargazer)

data_day1_ready <- readRDS("data/day1_ready.rds")
data_day30_ready <- readRDS("data/day30_ready.rds")

#### ДЕНЬ 1 ####

#### ПОРОГ 5% ####

#### === ФИНАЛЬНЫЕ МОДЕЛИ === ####

## Board size
model_full_BS <- glm(UNDP_5 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS <- glm(formula(model_step_BS), family = "binomial", data = data_day1_ready)

## Independence
model_full_In <- glm(UNDP_5 ~ Independence + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence))
In <- glm(formula(model_step_In), family = "binomial", data = data_day1_ready)

## Mean age
model_full_MA <- glm(UNDP_5 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA <- glm(formula(model_step_MA), family = "binomial", data = data_day1_ready)

## Gender variability
model_full_GV <- glm(UNDP_5 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV <- glm(formula(model_step_GV), family = "binomial", data = data_day1_ready)

## Independent leader
model_full_IL <- glm(UNDP_5 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL <- glm(formula(model_step_IL), family = "binomial", data = data_day1_ready)

## Current exp
model_full_CE <- glm(UNDP_5 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE <- glm(formula(model_step_CE), family = "binomial", data = data_day1_ready)

## Past exp
model_full_PE <- glm(UNDP_5 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE <- glm(formula(model_step_PE), family = "binomial", data = data_day1_ready)

## Number of committees
model_full_NC <- glm(UNDP_5 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC <- glm(formula(model_step_NC), family = "binomial", data = data_day1_ready)

## Foreign directors
model_full_FD <- glm(UNDP_5 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD <- glm(formula(model_step_FD), family = "binomial", data = data_day1_ready)

#### === Выгружаем === ####

stargazer(BS, In, MA, GV, IL, CE, PE, NC, FD,
          type = "html",
          out = "output/logit_regressions_one_var/day1_ready_log5.html",
          title = "Логистические регрессии (5% порог) по чистым данным (day1)")

#### ПОРОГ 5% ####

data_day1_ready <- readRDS("data/day1_ready.rds")

#### === ФИНАЛЬНЫЕ МОДЕЛИ === ####

## Board size
model_full_BS <- glm(UNDP_10 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS <- glm(formula(model_step_BS), family = "binomial", data = data_day1_ready)

## Independence
model_full_In <- glm(UNDP_10 ~ Independence + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence))
In <- glm(formula(model_step_In), family = "binomial", data = data_day1_ready)

## Mean age
model_full_MA <- glm(UNDP_10 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA <- glm(formula(model_step_MA), family = "binomial", data = data_day1_ready)

## Gender variability
model_full_GV <- glm(UNDP_10 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV <- glm(formula(model_step_GV), family = "binomial", data = data_day1_ready)

## Independent leader
model_full_IL <- glm(UNDP_10 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL <- glm(formula(model_step_IL), family = "binomial", data = data_day1_ready)

## Current exp
model_full_CE <- glm(UNDP_10 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE <- glm(formula(model_step_CE), family = "binomial", data = data_day1_ready)

## Past exp
model_full_PE <- glm(UNDP_10 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE <- glm(formula(model_step_PE), family = "binomial", data = data_day1_ready)

## Number of committees
model_full_NC <- glm(UNDP_10 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC <- glm(formula(model_step_NC), family = "binomial", data = data_day1_ready)

## Foreign directors
model_full_FD <- glm(UNDP_10 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day1_ready)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD <- glm(formula(model_step_FD), family = "binomial", data = data_day1_ready)

#### === Выгружаем === ####

stargazer(BS, In, MA, GV, IL, CE, PE, NC, FD,
          type = "html",
          out = "output/logit_regressions_one_var/day1_ready_log10.html",
          title = "Логистические регрессии (10% порог) по чистым данным (day1)")

#### ДЕНЬ 30 ####

#### ПОРОГ 5% ####

#### === ФИНАЛЬНЫЕ МОДЕЛИ === ####

## Board size
model_full_BS <- glm(UNDP_5 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS <- glm(formula(model_step_BS), family = "binomial", data = data_day30_ready)

## Independence
model_full_In <- glm(UNDP_5 ~ Independence + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence))
In <- glm(formula(model_step_In), family = "binomial", data = data_day30_ready)

## Mean age
model_full_MA <- glm(UNDP_5 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA <- glm(formula(model_step_MA), family = "binomial", data = data_day30_ready)

## Gender variability
model_full_GV <- glm(UNDP_5 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV <- glm(formula(model_step_GV), family = "binomial", data = data_day30_ready)

## Independent leader
model_full_IL <- glm(UNDP_5 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL <- glm(formula(model_step_IL), family = "binomial", data = data_day30_ready)

## Current exp
model_full_CE <- glm(UNDP_5 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE <- glm(formula(model_step_CE), family = "binomial", data = data_day30_ready)

## Past exp
model_full_PE <- glm(UNDP_5 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE <- glm(formula(model_step_PE), family = "binomial", data = data_day30_ready)

## Number of committees
model_full_NC <- glm(UNDP_5 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC <- glm(formula(model_step_NC), family = "binomial", data = data_day30_ready)

## Foreign directors
model_full_FD <- glm(UNDP_5 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD <- glm(formula(model_step_FD), family = "binomial", data = data_day30_ready)

#### === Выгружаем === ####

stargazer(BS, In, MA, GV, IL, CE, PE, NC, FD,
          type = "html",
          out = "output/logit_regressions_one_var/day30_ready_log5.html",
          title = "Логистические регрессии (5% порог) по чистым данным (day30)")

#### ПОРОГ 5% ####

#### === ФИНАЛЬНЫЕ МОДЕЛИ === ####

## Board size
model_full_BS <- glm(UNDP_10 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS <- glm(formula(model_step_BS), family = "binomial", data = data_day30_ready)

## Independence
model_full_In <- glm(UNDP_10 ~ Independence + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence))
In <- glm(formula(model_step_In), family = "binomial", data = data_day30_ready)

## Mean age
model_full_MA <- glm(UNDP_10 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA <- glm(formula(model_step_MA), family = "binomial", data = data_day30_ready)

## Gender variability
model_full_GV <- glm(UNDP_10 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV <- glm(formula(model_step_GV), family = "binomial", data = data_day30_ready)

## Independent leader
model_full_IL <- glm(UNDP_10 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL <- glm(formula(model_step_IL), family = "binomial", data = data_day30_ready)

## Current exp
model_full_CE <- glm(UNDP_10 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE <- glm(formula(model_step_CE), family = "binomial", data = data_day30_ready)

## Past exp
model_full_PE <- glm(UNDP_10 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE <- glm(formula(model_step_PE), family = "binomial", data = data_day30_ready)

## Number of committees
model_full_NC <- glm(UNDP_10 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC <- glm(formula(model_step_NC), family = "binomial", data = data_day30_ready)

## Foreign directors
model_full_FD <- glm(UNDP_10 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, 
                     family = "binomial", data = data_day30_ready)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD <- glm(formula(model_step_FD), family = "binomial", data = data_day30_ready)

#### === Выгружаем === ####

stargazer(BS, In, MA, GV, IL, CE, PE, NC, FD,
          type = "html",
          out = "output/logit_regressions_one_var/day30_ready_log10.html",
          title = "Логистические регрессии (10% порог) по чистым данным (day30)")