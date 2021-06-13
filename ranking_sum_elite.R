# For Linux
# Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
# For Windows
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

library(dplyr)
library(xlsx)
require(googledrive)
library(googlesheets4)

ranking_type = "elite"  # "elite", "sprint", 'mtbo' or 'ski' are available
max_amount_of_starts_counted_for_sum = 7

coefs_comps <- data.frame()

# Возможно, попросит аутентификации в браузере!
reference_database <- as.data.frame(read_sheet(drive_find(pattern = "Orienteers database",
                                                          type = "spreadsheet", n_max=1),
                                               sheet = format(Sys.Date(), "%Y"),
                                               col_types='ccciccccccccci'))

if(ranking_type == "elite") {
  googlesheet_name <- "Elite Ranking Starts"
} else {
  if(ranking_type == "sprint") {
    googlesheet_name <- "Sprint Ranking Starts"
  } else {
    if(ranking_type == "mtbo") {
      googlesheet_name <- "MTBO Ranking Starts"
    } else {
      if(ranking_type == "ski") {
        googlesheet_name <- "Ski Ranking Starts"
      } else {
        stop("Unsupported rating type!")
      }
    }
  }
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
reference_database <- reference_database[! is.na(reference_database$`Член БФО в текущем году`), ]
# Выбираем только нужные колонки из базы
reference_database <- reference_database[, c("ФИ", "ГР", "Группа")]
# Так как в элитном рейтинге могут принимать участие спортсмены из любой группы, оставляем только первую букву группы (= пол)
reference_database$`Группа` <- substr(reference_database$`Группа`, 1, 1)

sum <- left_join(reference_database, results_sum, by = c("ФИ", "ГР"))

# Сортируем
sum <- sum[order(sum$Группа, -sum$Сумма), ]

# Переименовываем колонки с очками так, как больше нравится Диме Давидовичу
colnames(sum)[grep("Очки_\\d{8}", colnames(sum))] <- format(strptime(substring(colnames(sum)[grep("Очки_\\d{8}", colnames(sum))], 10, 14), format = "%m%d"), format = "%d.%m")

# Убираем людей, которые в элитном рейтинге участия не принимали (у них в сумме не 0, а NA)
sum = filter(sum, ! is.na(Сумма))

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
