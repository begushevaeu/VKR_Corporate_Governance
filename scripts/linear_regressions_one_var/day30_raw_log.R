library(dplyr)
library(MASS)
library(stargazer)

#### === Загрузка данных === ####

data_BS <- readRDS("data/regressions/raw_log/day30/day30_Board_size_log_lin.rds")
data_In <- readRDS("data/regressions/raw_log/day30/day30_Independence_lin.rds")
data_MA <- readRDS("data/regressions/raw_log/day30/day30_Mean_age_log_lin.rds")
data_GV <- readRDS("data/regressions/raw_log/day30/day30_Gender_variability_lin.rds")
data_IL <- readRDS("data/regressions/raw_log/day30/day30_Independent_leader_lin.rds")
data_CE <- readRDS("data/regressions/raw_log/day30/day30_Current_exp_log_lin.rds")
data_PE <- readRDS("data/regressions/raw_log/day30/day30_Past_exp_log_lin.rds")
data_NC <- readRDS("data/regressions/raw_log/day30/day30_Number_of_committees_lin.rds")
data_FD <- readRDS("data/regressions/raw_log/day30/day30_Foreign_directors_lin.rds")

#### === ФИНАЛЬНЫЕ МОДЕЛИ === ####

## Board size
model_full_BS <- lm(Underpricing_30 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_BS)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS <- lm(formula(model_step_BS), data = data_BS)

## Independence
model_full_In <- lm(Underpricing_30 ~ Independence + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_In)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence))
In <- lm(formula(model_step_In), data = data_In)

## Mean age
model_full_MA <- lm(Underpricing_30 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_MA)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA <- lm(formula(model_step_MA), data = data_MA)

## Gender variability
model_full_GV <- lm(Underpricing_30 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_GV)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV <- lm(formula(model_step_GV), data = data_GV)

## Independent leader
model_full_IL <- lm(Underpricing_30 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_IL)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL <- lm(formula(model_step_IL), data = data_IL)

## Current exp
model_full_CE <- lm(Underpricing_30 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_CE)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE <- lm(formula(model_step_CE), data = data_CE)

## Past exp
model_full_PE <- lm(Underpricing_30 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_PE)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE <- lm(formula(model_step_PE), data = data_PE)

## Number of committees
model_full_NC <- lm(Underpricing_30 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_NC)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC <- lm(formula(model_step_NC), data = data_NC)

## Foreign directors
model_full_FD <- lm(Underpricing_30 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_FD)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD <- lm(formula(model_step_FD), data = data_FD)

#### === Выгружаем === ####

if (!dir.exists("output/linear_regressions_one_var")) {
  dir.create("output/linear_regressions_one_var")
}

stargazer(BS, In, MA, GV, IL, CE, PE, NC, FD,
          type = "html",
          out = "output/linear_regressions_one_var/day30_raw_log.html",
          title = "Линейные регрессии по грязным данным (day30)")