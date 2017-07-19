
library(googlesheets)

# Шапка файлов результатов должна быть следующей
# ФИ;Коллектив;Квал;Номер;ГР;Результат;Место;Прим;Группа

competition_date <- readline()

# Читаем список всех доступных файлов
# возможно, попросит аутентификации в браузере!
my_sheets <- gs_ls()

# Ищем тот документ, который соответствует дате
results_sheet_name <- my_sheets$sheet_title[grepl(pattern = paste0("^", competition_date), x = my_sheets$sheet_title)]

results_sheet <- gs_title(results_sheet_name)

# list worksheets (not necessary here as we have only one sheet for every results file)
gs_ws_ls(results_sheet)

# Читаем файл результатов
results <- as.data.frame(gs_read(results_sheet))

# Or do it all locally
# results_filename <- file.choose()
# 
# results <- read.csv2(results_filename, encoding = "UTF-8",
#                      colClasses = c("character", "character", "character", "integer",
#                                     "integer", "character", "character", "character",
#                                     "character", "character"))

library(stringi)
results$ФИ <- str_to_title(results$ФИ)

# У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
results$Место[results$Место == "в/к"] <- "0"
results$Место <- as.numeric(results$Место)
results$Результат_сек <- as.numeric(as.difftime(as.character(results$Результат), format = "%H:%M:%S", units = "secs"))

coefs_group <- read.csv2(file = "group_coef.txt", encoding = "UTF-8", stringsAsFactors = FALSE,
                         colClasses = c("character", "double"))

library(stringr)
results$Сложность <- substr(results$Группа, str_locate(results$Группа, paste0("(", paste0(coefs_group$Сложность, collapse = "|"), ")")), 1000)
library(dplyr)
results <- left_join(results, coefs_group, by = c("Сложность" = "Сложность"))

winning_time <- results %>% filter(Место != 0) %>% group_by(Группа) %>% summarise(Время_победителя = min(Результат_сек))

results <- left_join(results, winning_time, by = c("Группа" = "Группа"))

coefs_comps <- read.csv2(file = "2017_ranking_starts_and_coefs.txt", encoding = "UTF-8", stringsAsFactors = FALSE,
                        colClasses = c(rep("character", 4), "double"))

coef_comp <- coefs_comps$коэффициент[coefs_comps$дата == gsub(pattern = ".*([0-9]{8}).*", replacement = "\\1", x = results_filename)]

results <- mutate(results, Очки = round(coef_comp*1000*Коэффициент*(2* (Время_победителя / Результат_сек) - 1)))
results$Очки[results$Очки <= 1] <- 1
results$Очки[is.na(results$Очки)] <- 0
results$Очки[results$Место == 0] <- 0

# Выбираем только людей до группы 21 по возрасту
results <- filter(results, ГР >= 1997)

write.csv2(x = results, file = file.choose(), row.names = FALSE, fileEncoding = "UTF-8")

rm(coefs_group, results, winning_time)
