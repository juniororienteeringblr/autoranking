# For Linux
# Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
# For Windows
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

library(dplyr)
library(xlsx)
require(googledrive)
library(googlesheets4)

ranking_type = "junior"
max_amount_of_starts_counted_for_sum = 5

coefs_comps <- data.frame()

# Возможно, попросит аутентификации в браузере!
reference_database <- as.data.frame(read_sheet(drive_find(pattern = "Orienteers database",
                                                          type = "spreadsheet", n_max=1),
                                               sheet = format(Sys.Date(), "%Y"),
                                               col_types='cccicccccccc'))

reference_database$`ФИ` <- str_to_title(reference_database$`ФИ`)
reference_database$`ФИ` <- str_replace_all(reference_database$`ФИ`,
                                           stri_enc_toutf8("\U0451"), # ё
                                           stri_enc_toutf8("\U0435")) # е
reference_database$`ФИ` <- str_replace_all(reference_database$`ФИ`,
                                           stri_enc_toutf8("\U0401"), # Ё
                                           stri_enc_toutf8("\U0415")) # Е

if(ranking_type == "junior") {
  googlesheet_name <- "Junior Ranking Starts"
} else {
  stop("Unsupported rating type!")
}

coefs_comps <- as.data.frame(read_sheet(drive_find(pattern = googlesheet_name,
                                                   type = "spreadsheet"),
                                        sheet = format(Sys.Date(), "%Y")))

result_list <- list() #create an empty list

passed_comps <- coefs_comps[!is.na(coefs_comps$`Ссылка на результаты`), ]

for (i in 1:nrow(passed_comps)) {
  # Ищем тот документ, который соответствует дате
  results_filename <- paste0(passed_comps$Дата[i], "_", passed_comps$Название[i], "_", passed_comps$Вид[i])
  results_sheet <- drive_find(pattern = results_filename, type = "spreadsheet")
  
  # Читаем файл результатов
  sheet_name = paste0("scores_", ranking_type, "_ranking")
  
  result_list[[i]] <- as.data.frame(read_sheet(ss = results_sheet, sheet=sheet_name, col_types='ccciiciccicii'))
}
all_comps_results <- do.call("rbind",result_list) #combine all vectors into a matrix

all_comps_results$`ФИ` <- str_to_title(all_comps_results$`ФИ`)
all_comps_results$`ФИ` <- str_replace_all(all_comps_results$`ФИ`,
                                          stri_enc_toutf8("\U0451"), # ё
                                          stri_enc_toutf8("\U0435")) # е
all_comps_results$`ФИ` <- str_replace_all(all_comps_results$`ФИ`,
                                          stri_enc_toutf8("\U0401"), # Ё
                                          stri_enc_toutf8("\U0415")) # Е

for(i in 1:nrow(passed_comps)) {
  result_list[[i]] <- result_list[[i]][, c("ФИ", "ГР", "Очки")]
  names(result_list[[i]])[names(result_list[[i]]) == 'Очки'] <- paste0("Очки_", passed_comps$Дата[i])
}

results_sum <- data.frame(ФИ = character(), ГР = integer())

for(i in 1:nrow(passed_comps)) {
  results_sum <- full_join(x = results_sum, result_list[[i]], by = c("ФИ" = "ФИ", "ГР" = "ГР"))
}

# Теперь можно считать сумму
results_sum$Сумма <- apply(X = select(results_sum, starts_with("Очки")),
                           MARGIN = 1,
                           FUN = function(x) {sum(sort(x, decreasing = TRUE)[1:ifelse(length(x) < max_amount_of_starts_counted_for_sum, length(x), max_amount_of_starts_counted_for_sum)], na.rm = TRUE)})

results_sum$Среднее <- apply(X = select(results_sum, starts_with("Очки")),
                             MARGIN = 1,
                             FUN = function(x) {round(mean(sort(x, decreasing = TRUE)[1:ifelse(length(x) < max_amount_of_starts_counted_for_sum, length(x), max_amount_of_starts_counted_for_sum)], na.rm = TRUE))})

# Оставляем в базе только членов БФО
reference_database <- reference_database[! is.na(reference_database$`Членство БФО в текущем году`), ]
# Выбираем только нужные колонки из базы
reference_database$Возраст <- as.integer(format(Sys.Date(), "%Y")) - reference_database$ГР
reference_database$Возраст <- sapply(reference_database$Возраст,
                                     function(x) {ifelse(x < 21, x + x %% 2, ifelse(x >= 35, x - x %% 5, 21))})
reference_database$Группа <- paste0(reference_database$Пол, reference_database$Возраст)

reference_database <- reference_database[, c("ФИ", "ГР", "Группа")]

sum <- left_join(reference_database, results_sum, by = c("ФИ", "ГР"))

# Сортируем
sum <- sum[order(sum$Группа, -sum$Сумма), ]

# Переименовываем колонки с очками так, как больше нравится Диме Давидовичу
colnames(sum)[grep("Очки_\\d{8}", colnames(sum))] <- format(strptime(substring(colnames(sum)[grep("Очки_\\d{8}", colnames(sum))], 10, 14), format = "%m%d"), format = "%d.%m")

# Убираем неподходящие группы
if (ranking_type == "junior") {
  sum = filter(sum, Группа %in% c("М20", "Ж20"))
}

filename = paste0(ranking_type, "_ranking_sum_by_date_", last(passed_comps$Дата), ".xlsx")

for(i in sort(unique(sum$Группа))) {
  if(!file.exists(filename)) {
    x = filter(sum, Группа == i)
    x = cbind(`№` = 1:nrow(x), x, Место = 1:nrow(x))
    x$`Коллектив` <- NULL
    x$`Квал` <- NULL
    x$`Группа` <- NULL
    x <- x[! is.na(x$`Сумма`), ]
    write.xlsx(x, file = filename,
               sheetName = i, row.names = FALSE, showNA = FALSE)
  } else {
    x = filter(sum, Группа == i)
    x = cbind(`№` = 1:nrow(x), x, Место = 1:nrow(x))
    x$`Коллектив` <- NULL
    x$`Квал` <- NULL
    x$`Группа` <- NULL
    x <- x[! is.na(x$`Сумма`), ]
    write.xlsx(x, file = filename, append = TRUE,
               sheetName = i, row.names = FALSE, showNA = FALSE)
  }
}
