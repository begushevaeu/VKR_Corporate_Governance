library(dplyr)
library(stargazer)

#### === Загрузка данных === ####

data_day1_ready <- readRDS("data/day1_ready.rds")

#### === 1 день === ####

#### === 5% === ####

#### === Определяем переменные === ####

target_var <- "UNDP_5"

interest_vars <- c("Mean_age_log", "Board_size_log", "Gender_variability",
                   "Independence", "Independent_leader",
                   "Current_exp_log", "Past_exp_log", 
                   "Number_of_committees", "Foreign_directors")

control_vars <- c("Total_assets_log", "Age_of_company_log", 
                  "Year_hot", "Tech_sector", 
                  "ROA", "Net_Debt_EBITDA", "Current_ratio", 
                  "EBITDA_Margin")

#### === Функция проверки p-value < 0.05 === ####

is_significant_005 <- function(coef_table, term) {
  if (term %in% rownames(coef_table)) {
    pval <- coef_table[term, 4]
    return(!is.na(pval) && pval < 0.05)
  }
  return(FALSE)
}

#### === Основной цикл === ####

selected_models <- list()

for (interest in interest_vars) {
  
  for (control in control_vars) {
    
    effect_name <- paste0(interest, ":", control)
    temp_data <- data_day1_ready
    temp_data[[effect_name]] <- temp_data[[interest]] * temp_data[[control]]
    
    formula <- as.formula(
      paste(target_var, "~", 
            paste(c(interest, control_vars, effect_name), collapse = " + "))
    )
    
    model <- glm(formula, family = "binomial", data = temp_data)
    
    coefs <- summary(model)$coefficients
    
    # Проверяем, что обе переменные значимы на уровне 5%
    if (is_significant_005(coefs, interest) & is_significant_005(coefs, effect_name)) {
      
      model_name <- paste(interest, control, sep = "_x_")
      selected_models[[model_name]] <- model
      
    }
  }
}

#### === СОХРАНЯЕМ === ####

if (!dir.exists("output/logit_regressions_interactions")) {
  dir.create("output/logit_regressions_interactions")
}

stargazer(selected_models,
          type = "html",
          out = "output/logit_regressions_interactions/day1_ready_UNDP_5_interactions_p05.html",
          title = "Логистические модели (чистые данные, день 1, UNDP_5): ПИ и взаимодействие значимы на 5%")

#### === 10% === ####

#### === Определяем переменные === ####

target_var <- "UNDP_10"

interest_vars <- c("Mean_age_log", "Board_size_log", "Gender_variability",
                   "Independence", "Independent_leader",
                   "Current_exp_log", "Past_exp_log", 
                   "Number_of_committees", "Foreign_directors")

control_vars <- c("Total_assets_log", "Age_of_company_log", 
                  "Year_hot", "Tech_sector", 
                  "ROA", "Net_Debt_EBITDA", "Current_ratio", 
                  "EBITDA_Margin")

#### === Функция проверки p-value < 0.05 === ####

is_significant_005 <- function(coef_table, term) {
  if (term %in% rownames(coef_table)) {
    pval <- coef_table[term, 4]
    return(!is.na(pval) && pval < 0.05)
  }
  return(FALSE)
}

#### === Основной цикл === ####

selected_models <- list()

for (interest in interest_vars) {
  
  for (control in control_vars) {
    
    effect_name <- paste0(interest, ":", control)
    temp_data <- data_day1_ready
    temp_data[[effect_name]] <- temp_data[[interest]] * temp_data[[control]]
    
    formula <- as.formula(
      paste(target_var, "~", 
            paste(c(interest, control_vars, effect_name), collapse = " + "))
    )
    
    model <- glm(formula, family = "binomial", data = temp_data)
    
    coefs <- summary(model)$coefficients
    
    # Проверяем, что обе переменные значимы на уровне 5%
    if (is_significant_005(coefs, interest) & is_significant_005(coefs, effect_name)) {
      
      model_name <- paste(interest, control, sep = "_x_")
      selected_models[[model_name]] <- model
      
    }
  }
}

#### === СОХРАНЯЕМ === ####

if (!dir.exists("output/logit_regressions_interactions")) {
  dir.create("output/logit_regressions_interactions")
}

stargazer(selected_models,
          type = "html",
          out = "output/logit_regressions_interactions/day1_ready_UNDP_10_interactions_p05.html",
          title = "Логистические модели (чистые данные, день 1, UNDP_10): ПИ и взаимодействие значимы на 5%")

