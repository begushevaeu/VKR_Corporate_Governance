library(dplyr)
library(stargazer)

#### === Загрузка данных === ####

data_day30_ready <- readRDS("data/day30_ready.rds")

#### === Определяем переменные === ####

target_var <- "Underpricing_30"

interest_vars <- c("Mean_age_log", "Board_size_log", "Gender_variability",
                   "Independence", "Independent_leader",
                   "Current_exp_log", "Past_exp_log", 
                   "Number_of_committees", "Foreign_directors")

control_vars <- c("Total_assets_log", "Age_of_company_log", 
                  "Year_hot", "Tech_sector", 
                  "ROA", "Net_Debt_EBITDA", "Current_ratio", 
                  "EBITDA_Margin")

#### === Функция проверки значимости === ####

is_significant <- function(coef_table, term) {
  if (term %in% rownames(coef_table)) {
    pval <- coef_table[term, 4]
    return(!is.na(pval) && pval < 0.1)
  }
  return(FALSE)
}

#### === Основной цикл === ####

selected_models <- list()

for (interest in interest_vars) {
  
  for (control in control_vars) {
    
    effect_name <- paste0(interest, ":", control)
    temp_data <- data_day30_ready
    temp_data[[effect_name]] <- temp_data[[interest]] * temp_data[[control]]
    
    formula <- as.formula(
      paste(target_var, "~", 
            paste(c(interest, control_vars, effect_name), collapse = " + "))
    )
    
    model <- lm(formula, data = temp_data)
    
    coefs <- summary(model)$coefficients
    
    # Проверка значимости интереса И взаимодействия
    if (is_significant(coefs, interest) & is_significant(coefs, effect_name)) {
      
      model_name <- paste(interest, control, sep = "_x_")
      selected_models[[model_name]] <- model
      
    }
  }
}

#### === СОХРАНЯЕМ === ####

if (!dir.exists("output/linear_regressions_interactions")) {
  dir.create("output/linear_regressions_interactions")
}

stargazer(selected_models,
          type = "html",
          out = "output/linear_regressions_interactions/day30_ready_interactions.html",
          title = "Линейные модели (чистые данные, день 30): значимые ПИ и взаимодействия")