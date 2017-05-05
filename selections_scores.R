
results <- read.csv2(file = "results/20170401_Брестский_Подснежник_классика.csv", encoding = "UTF-8")

results$Результат_сек <- as.numeric(as.difftime(as.character(results$Результат), format = "%H:%M:%S", units = "secs"))

coefs <- read.csv2(file = "group_coef.txt", encoding = "UTF-8", stringsAsFactors = FALSE)
coefs$Коэффициент <- as.numeric(coefs$Коэффициент)

library(stringr)
results$Сложность <- substr(results$Группа, str_locate(results$Группа, paste0("(", paste0(coefs$Сложность, collapse = "|"), ")")), 1000)
library(dplyr)
results <- left_join(results, coefs, by = c("Сложность" = "Сложность"))

winning_time <- results %>% group_by(Группа) %>% summarise(Время_победителя = min(Результат_сек, na.rm = TRUE))

results <- left_join(results, winning_time, by = c("Группа" = "Группа"))

results$Результат_сек

results <- mutate(results, Очки = round(1000*Коэффициент*(2* (Время_победителя / Результат_сек) - 1)))
results$Очки[results$Очки <= 1] <- 1
results$Очки[is.na(results$Очки)] <- 0
