
# Шапка файлов результатов должна быть следующей
# ФИ;Коллектив;Квал;Номер;ГР;Результат;Место;Вып;Прим;Группа

results <- read.csv2(file.choose(), encoding = "UTF-8",
                     colClasses = c("character", "character", "character", "integer",
                                    "integer", "character", "character", "character",
                                    "character", "character"))

library(stringi)
results$ФИ <- str_to_title(results$ФИ)

# У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
results$Место[results$Место == "в/к"] <- "0"
results$Место <- as.numeric(results$Место)
results$Результат_сек <- as.numeric(as.difftime(as.character(results$Результат), format = "%H:%M:%S", units = "secs"))

coefs <- read.csv2(file = "group_coef.txt", encoding = "UTF-8", stringsAsFactors = FALSE,
                   colClasses = c("character", "double"))

library(stringr)
results$Сложность <- substr(results$Группа, str_locate(results$Группа, paste0("(", paste0(coefs$Сложность, collapse = "|"), ")")), 1000)
library(dplyr)
results <- left_join(results, coefs, by = c("Сложность" = "Сложность"))

winning_time <- results %>% filter(Место != 0) %>% group_by(Группа) %>% summarise(Время_победителя = min(Результат_сек))

results <- left_join(results, winning_time, by = c("Группа" = "Группа"))

results <- mutate(results, Очки = round(1000*Коэффициент*(2* (Время_победителя / Результат_сек) - 1)))
results$Очки[results$Очки <= 1] <- 1
results$Очки[is.na(results$Очки)] <- 0
results$Очки[results$Место == 0] <- 0

# Выбираем только людей до группы 21 по возрасту
results <- filter(results, ГР >= 1997)

write.csv2(x = results, file = file.choose(), row.names = FALSE, fileEncoding = "UTF-8")

rm(coefs, results, winning_time)
