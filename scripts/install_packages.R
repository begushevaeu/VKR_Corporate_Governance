#### Установка необходимых пакетов ####

# Список необходимыв пакетов
required_packages <- c(
  "dplyr", 
  "missForest", 
  "stargazer", 
  "lmtest", 
  "MASS",
  "scipub"
)

# Функция для установки пакета, если он ещё не установлен
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Установка всех пакетов из списка
invisible(sapply(required_packages, install_if_missing))