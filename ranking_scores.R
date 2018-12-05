ranking_scores <- function(results_source = c("googlesheets", "local"),
                           competition_date = NA,
                           competition_name = NA,
                           competition_distance = NA,
                           ranking_type = c("youth", "junior"),
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
  
  library(stringi)
  library(stringr)
  results$`ФИ` <- str_to_title(results$`ФИ`)
  results$`ФИ` <- str_replace_all(results$`ФИ`, c("ё" = "е", "Ё" = "Е"))
  
  # Убираем иностранных спортсменов
  results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)$", results$`ФИ`), ]
  results <- results[!grepl("(Rus)|(RUS)|(rus)|(EST)$", results$`Коллектив`), ]
  
  # У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
  results$`Место`[results$`Место` == "в/к"] <- "0"
  results$`Место` <- as.numeric(results$`Место`)
  results$`Результат_сек` <- as.numeric(as.difftime(as.character(results$`Результат`), format = "%H:%M:%S", units = "secs"))
  
  coefs_group <- read.csv2(file = "group_coef.txt", encoding = "UTF-8", stringsAsFactors = FALSE,
                           colClasses = c("character", "double"))
  
  results$`Сложность` <- substr(results$`Группа`, str_locate(results$`Группа`, paste0("(", paste0(coefs_group$`Сложность`, collapse = "|"), ")")), 1000)
  library(dplyr)
  results <- left_join(results, coefs_group, by = c("Сложность" = "Сложность"))
  
  winning_time <- results %>% filter(`Место` != 0) %>% group_by(`Группа`) %>% summarise(`Время_победителя` = min(`Результат_сек`))
  #TODO: может случиться ситуация, когда лидер по группе бежал в/к, тогда у нас не будет времени победителя - подумать после
  
  results <- left_join(results, winning_time, by = c("Группа" = "Группа"))
  
  results <- mutate(results, `Очки` = round(competition_coefficient*1000*`Коэффициент`*(2* (`Время_победителя` / `Результат_сек`) - 1)))
  # results$`Очки`[results$`Очки` <= 1] <- 1
  results$`Очки`[results$`Очки` < 1] <- 0
  results$`Очки`[is.na(results$`Очки`)] <- 0
  results$`Очки`[results$`Место` == 0] <- 0
  
  require(stringi)
  names(results) <- stri_encode(names(results), "", "UTF-8")
  
  results <- as.data.frame(results)
  
  # Выбираем только людей подходящих групп по возрасту
  if(ranking_type == "youth") {
    results <- results[as.integer(format(Sys.Date(), "%Y")) - results$`ГР` <= 18, ]
    if(results_source == "googlesheets") {
      if("scores_youth_ranking" %in% gs_ws_ls(results_sheet)) {
        results_sheet <- results_sheet %>%
          gs_ws_delete(ws = "scores_youth_ranking", verbose = FALSE)
      }
      results_sheet <- results_sheet %>%
        gs_ws_new(ws_title = "scores_youth_ranking", input = results, verbose = TRUE)
    } else {
      if(results_source == "local") {
        # Or do it all locally
        results_score_filename <- paste0(str_replace(pattern = "\\.csv", string = results_filename, replacement = ""),
                                         "_scores_youth_ranking.csv")
        write.csv2(x = results, file = file.path(getwd(), "results", results_score_filename),
                   row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        stop("Unsupported results source!")
      }
    }
    
  } else {
    if(ranking_type == "junior") {
      results <- results[between(as.integer(format(Sys.Date(), "%Y")) - results$`ГР`, 19, 20), ]
      if(results_source == "googlesheets") {
        if("scores_junior_ranking" %in% gs_ws_ls(results_sheet)) {
          results_sheet <- results_sheet %>%
            gs_ws_delete(ws = "scores_junior_ranking", verbose = FALSE)
        }
        results_sheet <- results_sheet %>%
          gs_ws_new(ws_title = "scores_junior_ranking", input = results, verbose = TRUE)
      } else {
        if(results_source == "local") {
          # Or do it all locally
          results_score_filename <- paste0(str_replace(pattern = "\\.csv", string = results_filename, replacement = ""),
                                           "_scores_junior_ranking.csv")
          write.csv2(x = results, file = file.path(getwd(), "results", results_score_filename),
                     row.names = FALSE, fileEncoding = "UTF-8")
        } else {
          stop("Unsupported results source!")
        }
      }
    } else {
      stop("Unsupported ranking type!")
    }
  }
  
  rm(coefs_group, results, winning_time)
  
}
