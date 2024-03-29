ranking_scores_junior <- function(competition_date = NA,
                           competition_name = NA,
                           competition_distance = NA,
                           ranking_type = c("junior"),
                           competition_coefficient = NA) {
  # Шапка файлов результатов должна быть следующей
  # ФИ;Коллектив;Квал;Номер;ГР;Результат;Место;Прим;Группа
  
  print(paste("Computing ranking for date:", competition_date, "and coefficient", competition_coefficient))

  require(googlesheets4)
  library(stringi)
  library(stringr)
  
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
                                                 col_types='cccicccccccc'))
  
  results$`ФИ` <- str_to_title(results$`ФИ`)
  results$`ФИ` <- str_replace_all(results$`ФИ`,
                                  stri_enc_toutf8("\U0451"), # ё
                                  stri_enc_toutf8("\U0435")) # е
  results$`ФИ` <- str_replace_all(results$`ФИ`,
                                  stri_enc_toutf8("\U0401"), # Ё
                                  stri_enc_toutf8("\U0415")) # Е
  
  reference_database$`ФИ` <- str_to_title(reference_database$`ФИ`)
  reference_database$`ФИ` <- str_replace_all(reference_database$`ФИ`,
                                             stri_enc_toutf8("\U0451"), # ё
                                             stri_enc_toutf8("\U0435")) # е
  reference_database$`ФИ` <- str_replace_all(reference_database$`ФИ`,
                                             stri_enc_toutf8("\U0401"), # Ё
                                             stri_enc_toutf8("\U0415")) # Е
  
  # # Убираем иностранных спортсменов
  # results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`ФИ`), ]
  # results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`Коллектив`), ]
  
  library(dplyr)
  # Убираем спортсменов-не членов федерации
  reference_database <- reference_database[! is.na(reference_database$`Членство БФО в текущем году`), ]
  # Убираем тех, кто оплатил взнос после даты соревнований
  reference_database <- reference_database[as.Date(reference_database$`Дата оплаты`, '%d.%m.%Y') <= as.Date(competition_date, '%Y%m%d'), ]
  filtered_out <- anti_join(results, reference_database, by = c("ФИ", "ГР"))
  results <- semi_join(results, reference_database, by = c("ФИ", "ГР"))
  
  # У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
  results$`Место`[results$`Место` == "в/к"] <- "0"
  results$`Место` <- as.numeric(results$`Место`)
  results$`Результат_сек` <- as.numeric(as.difftime(as.character(results$`Результат`), format = "%H:%M:%S", units = "secs"))
  
  results$`Сложность` <- substr(results$`Группа`, 2, 1000)
  # Выбираем только группу 20 и сильнейшую из элитных групп
  # ЕЩЕ НЕ СДЕЛАНО, СДЕЛАТЬ!!!
  results <- results %>% filter(`Сложность` == stri_enc_toutf8("\u0032\u0030") | # 20
                                  `Сложность` == stri_enc_toutf8("\u0032\u0031") | # 21
                                  `Сложность` == stri_enc_toutf8("\u0032\u0031\u0415") | # 21Е
                                  `Сложность` == stri_enc_toutf8("\u0032\u0031\u0045")) # 21E
  
  library(dplyr)
  # results <- left_join(results, coefs_group, by = c("Сложность" = "Сложность"))
  
  # И чтобы не было в/к шников удаляем вообще их результаты
  results <- results %>% filter(`Место` != "0")
  
  winning_time <- results %>% filter(`Место` != 0) %>% group_by(`Группа`) %>% filter(!is.na(`Результат_сек`)) %>% summarise(`Время_победителя` = min(`Результат_сек`))

  results <- left_join(results, winning_time, by = c("Группа" = "Группа"))
  
  # results <- mutate(results, `Очки` = round(competition_coefficient*1000*`Коэффициент`*(2* (`Время_победителя` / `Результат_сек`) - 1)))
  results <- mutate(results, `Очки` = round(competition_coefficient*(2* (`Время_победителя` / `Результат_сек`) - 1)))
  # results$`Очки`[results$`Очки` <= 1] <- 1
  results$`Очки`[results$`Очки` < 1] <- 0
  results$`Очки`[is.na(results$`Очки`)] <- 0
  results$`Очки`[results$`Место` == 0] <- 0
  
  results <- as.data.frame(results)
  filtered_out <- as.data.frame(filtered_out)
  
  # Выбираем только людей подходящих групп по возрасту
  results <- results[between(as.integer(format(Sys.Date(), "%Y")) - results$`ГР`, 19, 20), ]
  filtered_out <- filtered_out[between(as.integer(format(Sys.Date(), "%Y")) - filtered_out$`ГР`, 19, 20), ]
  
  sheet_name = paste0("scores_", ranking_type, "_ranking")
  if(sheet_name %in% sheet_names(results_sheet)) {
    sheet_delete(results_sheet, sheet_name)
  }
  sheet_write(results, ss = results_sheet, sheet = sheet_name)
  
  filtered_out_sheet_name = paste0("filtered_out_", ranking_type, "_ranking")
  if(filtered_out_sheet_name %in% sheet_names(results_sheet)) {
    sheet_delete(results_sheet, filtered_out_sheet_name)
  }
  sheet_write(filtered_out, ss = results_sheet, sheet = filtered_out_sheet_name)
  
  rm(coefs_group, results, filtered_out, winning_time)
}
