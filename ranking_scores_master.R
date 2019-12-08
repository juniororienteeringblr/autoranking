ranking_scores_master <- function(results_source = c("googlesheets", "local"),
                           competition_date = NA,
                           competition_name = NA,
                           competition_distance = NA,
                           ranking_type = c("master"),
                           competition_coefficient = NA) {
  # Шапка файлов результатов должна быть следующей
  # ФИ;Коллектив;Квал;Номер;ГР;Результат;Место;Прим;Группа
  
  if(is.na(competition_date)) {
    print("Enter competitions date: ")
    competition_date <- as.character(readline())
  }
  
  if(is.na(competition_coefficient)) {
    print("Enter competitions coefficient: ")
    competition_coefficient <- as.numeric(readline())
  }
  
  print(paste("Computing ranking for date:", competition_date, "and coefficient", competition_coefficient))
  
  if(results_source == "googlesheets") {
    require(googlesheets)
    
    # Читаем список всех доступных файлов
    # возможно, попросит аутентификации в браузере!
    my_sheets <- gs_ls()
    
    # Ищем тот документ, который соответствует дате
    results_filename <- my_sheets$sheet_title[grepl(pattern = paste0("^", competition_date, "_", competition_name, "_", competition_distance),
                                                    x = my_sheets$sheet_title)]
    
    print(results_filename)
    
    results_sheet <- gs_title(results_filename)
    
    # list worksheets (not necessary here as we have only one sheet for every results file)
    gs_ws_ls(results_sheet)
    
    # Читаем файл результатов
    results <- as.data.frame(gs_read(ss = results_sheet))
  } else {
    if(results_source == "local") {
      # Or do it all locally
      results_filename <- paste0(competition_date, "_", competition_name, "_", competition_distance, ".csv")
      results <- read.csv2(file.path(getwd(), "results", results_filename), encoding = "UTF-8",
                           colClasses = c("character", "character", "character", "integer",
                                          "integer", "character", "character", "character",
                                          "character"))
    } else {
      stop("Unsupported results source!")
    }
  }
  
  # Читаем базу ориентировщиков
  if(results_source == "googlesheets") {
    # Читаем список всех доступных файлов для будущего использования
    my_sheets <- gs_ls()
    
    reference_database <- as.data.frame(gs_read(gs_title("Orienteers database"), ws = format(Sys.Date(), "%Y")))
  } else {
    if(results_source == "local") {
      # Or do it all locally
      reference_database <- read.csv2(file = "youth_and_junior_database.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
    } else {
      stop("Unsupported results source!")
    }
  }
  
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
  
  require(stringi)
  names(results) <- stri_enc_toutf8(names(results), is_unknown_8bit = FALSE, validate = FALSE)
  
  results <- as.data.frame(results)
  
  # Выбираем только людей подходящих групп по возрасту
  results <- results[as.integer(format(Sys.Date(), "%Y")) - results$`ГР` >= 35, ]
  if(results_source == "googlesheets") {
    if("scores_master_ranking" %in% gs_ws_ls(results_sheet)) {
      results_sheet <- results_sheet %>%
        gs_ws_delete(ws = "scores_master_ranking", verbose = FALSE)
    }
    results_sheet <- results_sheet %>%
      gs_ws_new(ws_title = "scores_master_ranking", input = results, verbose = TRUE)
  } else {
    if(results_source == "local") {
      # Or do it all locally
      results_score_filename <- paste0(str_replace(pattern = "\\.csv", string = results_filename, replacement = ""),
                                       "_scores_master_ranking.csv")
      write.csv2(x = results, file = file.path(getwd(), "results", results_score_filename),
                 row.names = FALSE, fileEncoding = "UTF-8")
    } else {
      stop("Unsupported results source!")
    }
  }
  
  rm(results, winning_time)
}
