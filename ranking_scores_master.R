ranking_scores_master <- function(competition_date = NA,
                                  competition_name = NA,
                                  competition_distance = NA,
                                  ranking_type = c("master"),
                                  competition_coefficient = NA) {
  # Шапка файлов результатов должна быть следующей
  # ФИ;Коллектив;Квал;Номер;ГР;Результат;Место;Прим;Группа
  
  print(paste("Computing ranking for date:", competition_date, "and coefficient", competition_coefficient))
  
  require(googlesheets4)
  
  # Ищем тот документ, который соответствует дате
  print(paste("Searching for the file", paste0(competition_date, "_", competition_name, "_", competition_distance)))
  googlesheet_results_name = paste0(competition_date, "_", competition_name, "_", competition_distance)
  results_sheet <- drive_find(pattern = googlesheet_results_name, type = "spreadsheet")
  
  print(results_sheet)
  
  # Читаем файл результатов
  results <- as.data.frame(read_sheet(ss = results_sheet, sheet="results", col_types='ccciicccc'))
  
  # Читаем базу ориентировщиков
  reference_database <- as.data.frame(read_sheet(drive_find(pattern = "Orienteers database",
                                                            type = "spreadsheet", n_max=1),
                                                 sheet = format(Sys.Date(), "%Y"),
                                                 col_types='ccciccccccccc'))
  
  library(stringi)
  library(stringr)
  results$`ФИ` <- str_to_title(results$`ФИ`)
  results$`ФИ` <- str_replace_all(results$`ФИ`, stri_enc_toutf8("ё"), stri_enc_toutf8("е"))
  results$`ФИ` <- str_replace_all(results$`ФИ`, stri_enc_toutf8("Ё"), stri_enc_toutf8("Е"))
  
  # # Убираем иностранных спортсменов
  # results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`ФИ`), ]
  # results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`Коллектив`), ]
  
  library(dplyr)
  # Убираем спортсменов-не членов федерации
  reference_database <- reference_database[! is.na(reference_database$`Член БФО в текущем году`), ]
  results <- semi_join(results, reference_database, by = c("ФИ", "ГР"))
  
  # У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
  results$`Место`[results$`Место` == "в/к"] <- "0"
  results$`Место` <- as.numeric(results$`Место`)
  results$`Результат_сек` <- as.numeric(as.difftime(as.character(results$`Результат`), format = "%H:%M:%S", units = "secs"))
  
  results$`Сложность` <- substr(results$`Группа`, 2, 1000)
  
  # Убираем группы младше 35
  results <- results %>% filter(`Сложность` >= 35)
  # И чтобы не было в/к шников удаляем вообще их результаты
  results <- results %>% filter(`Место` != "0")
  
  winning_time <- results %>% filter(`Место` != 0) %>% group_by(`Группа`) %>% filter(!is.na(`Результат_сек`)) %>% summarise(`Время_победителя` = min(`Результат_сек`))
  
  results <- left_join(results, winning_time, by = c("Группа" = "Группа"))
  
  results <- mutate(results, `Очки` = round(competition_coefficient*(2* (`Время_победителя` / `Результат_сек`) - 1)))
  # results$`Очки`[results$`Очки` <= 1] <- 1
  results$`Очки`[results$`Очки` < 1] <- 0
  results$`Очки`[is.na(results$`Очки`)] <- 0
  results$`Очки`[results$`Место` == 0] <- 0
  
  names(results) <- stri_enc_toutf8(names(results), is_unknown_8bit = FALSE, validate = FALSE)
  
  results <- as.data.frame(results)
  
  # Выбираем только людей подходящих групп по возрасту
  results <- results[as.integer(format(Sys.Date(), "%Y")) - results$`ГР` >= 35, ]
  
  if(paste0("scores_", ranking_type, "_ranking") %in% sheet_names(results_sheet)) {
    sheet_delete(results_sheet, paste0("scores_", ranking_type, "_ranking"))
  }
  sheet_write(results, ss = results_sheet, sheet = paste0("scores_", ranking_type, "_ranking"))
  
  rm(results, winning_time)
}
