# For Linux
# Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
# For Windows
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

source("ranking_scores.R", encoding = "UTF-8")

results_source = "googlesheets"
ranking_type = "junior"

coefs_comps <- data.frame()

if(results_source == "googlesheets") {
  # Возможно, попросит аутентификации в браузере!
  require(googlesheets)
  
  if(ranking_type == "youth") {
    coefs_comps <- as.data.frame(gs_read(gs_title(paste(format(Sys.Date(), "%Y"), "Youth Ranking Starts"))))
    coefs_comps$Дата <- as.character(coefs_comps$Дата)
  } else {
    if(ranking_type == "junior") {
      coefs_comps <- as.data.frame(gs_read(gs_title(paste(format(Sys.Date(), "%Y"), "Junior Ranking Starts"))))
      coefs_comps$Дата <- as.character(coefs_comps$Дата)
    } else {
      stop("Unsupported rating type!")
    }
  }
} else {
  if(results_source == "local") {
    # Or do it all locally
    if(ranking_type == "youth") {
      print("Выберите файл с описанием рейтинговых стартов и их коэффициентов для юношеского рейтинга.")
      coefs_comps_filename <- file.choose()
      coefs_comps <- read.csv2(coefs_comps_filename, encoding = "UTF-8", stringsAsFactors = FALSE,
                               colClasses = c(rep("character", 4), "double"))
    } else {
      if(ranking_type == "junior") {
        print("Выберите файл с описанием рейтинговых стартов и их коэффициентов для юниорского рейтинга.")
        coefs_comps_filename <- file.choose()
        coefs_comps <- read.csv2(coefs_comps_filename, encoding = "UTF-8", stringsAsFactors = FALSE,
                                 colClasses = c(rep("character", 4), "double"))
      } else {
        stop("Unsupported rating type!")
      }
    }
  } else {
    stop("Unsupported results source!")
  }
}

passed_comps <- coefs_comps[!is.na(coefs_comps$`Ссылка на результаты`), ]

apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
  ranking_scores(results_source = results_source,
                 competition_date = x["Дата"],
                 competition_name = x["Название"],
                 competition_distance = x["Вид"],
                 ranking_type = ranking_type,
                 competition_coefficient = as.numeric(x["Коэффициент"]))
})
