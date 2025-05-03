library(dplyr)
library(MASS)
library(stargazer)

#### === Загрузка данных === ####

data_day1_ready <- readRDS("data/day1_ready.rds")

## Board size
model_full_BS <- lm(Underpricing_1 ~ Board_size_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_BS <- stepAIC(model_full_BS, direction = "both", trace = TRUE, scope = list(lower = ~ Board_size_log))
BS1 <- lm(formula(model_step_BS), data = data_day1_ready)

## Independence
model_full_In <- lm(Underpricing_1 ~ Independence + I(Independence^2) + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_In <- stepAIC(model_full_In, direction = "both", trace = TRUE, scope = list(lower = ~ Independence + I(Independence^2)))
In1 <- lm(formula(model_step_In), data = data_day1_ready)

## Mean age
model_full_MA <- lm(Underpricing_1 ~ Mean_age_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_MA <- stepAIC(model_full_MA, direction = "both", trace = TRUE, scope = list(lower = ~ Mean_age_log))
MA1 <- lm(formula(model_step_MA), data = data_day1_ready)

## Gender variability
model_full_GV <- lm(Underpricing_1 ~ Gender_variability + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_GV <- stepAIC(model_full_GV, direction = "both", trace = TRUE, scope = list(lower = ~ Gender_variability))
GV1 <- lm(formula(model_step_GV), data = data_day1_ready)

## Independent leader
model_full_IL <- lm(Underpricing_1 ~ Independent_leader + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_IL <- stepAIC(model_full_IL, direction = "both", trace = TRUE, scope = list(lower = ~ Independent_leader))
IL1 <- lm(formula(model_step_IL), data = data_day1_ready)

## Current exp
model_full_CE <- lm(Underpricing_1 ~ Current_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_CE <- stepAIC(model_full_CE, direction = "both", trace = TRUE, scope = list(lower = ~ Current_exp_log))
CE1 <- lm(formula(model_step_CE), data = data_day1_ready)

## Past exp
model_full_PE <- lm(Underpricing_1 ~ Past_exp_log + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_PE <- stepAIC(model_full_PE, direction = "both", trace = TRUE, scope = list(lower = ~ Past_exp_log))
PE1 <- lm(formula(model_step_PE), data = data_day1_ready)

## Number of committees
model_full_NC <- lm(Underpricing_1 ~ Number_of_committees + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_NC <- stepAIC(model_full_NC, direction = "both", trace = TRUE, scope = list(lower = ~ Number_of_committees))
NC1 <- lm(formula(model_step_NC), data = data_day1_ready)

## Foreign directors
model_full_FD <- lm(Underpricing_1 ~ Foreign_directors + Total_assets_log + Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin, data = data_day1_ready)
model_step_FD <- stepAIC(model_full_FD, direction = "both", trace = TRUE, scope = list(lower = ~ Foreign_directors))
FD1 <- lm(formula(model_step_FD), data = data_day1_ready)

#### === Выгружаем === ####

stargazer(BS1, In1, MA1, GV1, IL1, CE1, PE1, NC1, FD1,
          type = "html",
          out = "output/linear_regressions_one_var/day1_ready.html",
          title = "Линейные регрессии по чистым данным (day1)")