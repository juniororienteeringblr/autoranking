# For Linux
# Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
# For Windows
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

source("ranking_scores.R", encoding = "UTF-8")
source("ranking_scores_master.R", encoding = "UTF-8")
source("ranking_scores_elite.R", encoding = "UTF-8")

results_source = "googlesheets"
ranking_type = "elite" # Available options: "youth", "junior", "master", "elite"

coefs_comps <- data.frame()

if(results_source == "googlesheets") {
  # Возможно, попросит аутентификации в браузере!
  require(googledrive)
  require(googlesheets4)
  
  if(ranking_type == "youth") {
    googlesheet_name <- "Youth Ranking Starts"
  } else {
    if(ranking_type == "junior") {
      googlesheet_name <- "Junior Ranking Starts"
    } else {
      if(ranking_type == "master") {
        googlesheet_name <- "Master Ranking Starts"
      } else {
        if(ranking_type == "elite") {
          googlesheet_name <- "Elite Ranking Starts"
        } else{
          stop("Unsupported rating type!")
        }
      }
    }
  }
  coefs_comps <- as.data.frame(read_sheet(drive_find(pattern = googlesheet_name, type = "spreadsheet", n_max=1), sheet = format(Sys.Date(), "%Y")))
  coefs_comps$Дата <- as.character(coefs_comps$Дата)
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
        if(ranking_type == "master") {
          print("Выберите файл с описанием рейтинговых стартов и их коэффициентов для ветеранского рейтинга.")
          coefs_comps_filename <- file.choose()
          coefs_comps <- read.csv2(coefs_comps_filename, encoding = "UTF-8", stringsAsFactors = FALSE,
                                   colClasses = c(rep("character", 4), "double"))
        } else {
          if(ranking_type == "elite") {
            print("Выберите файл с описанием рейтинговых стартов и их коэффициентов для элитного рейтинга.")
            coefs_comps_filename <- file.choose()
            coefs_comps <- read.csv2(coefs_comps_filename, encoding = "UTF-8", stringsAsFactors = FALSE,
                                     colClasses = c(rep("character", 4), "double"))
          } else {
          stop("Unsupported rating type!")
          }
        }
      }
    }
  } else {
    stop("Unsupported results source!")
  }
}

passed_comps <- coefs_comps[!is.na(coefs_comps$`Ссылка на результаты`), ]

if ((ranking_type == "youth") | (ranking_type == "junior")) {
  apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
    ranking_scores(results_source = results_source,
                   competition_date = x["Дата"],
                   competition_name = x["Название"],
                   competition_distance = x["Вид"],
                   ranking_type = ranking_type,
                   competition_coefficient = as.numeric(x["Коэффициент"]))
  })
} else {
  if(ranking_type == "master") {
    apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
      ranking_scores_master(results_source = results_source,
                            competition_date = x["Дата"],
                            competition_name = x["Название"],
                            competition_distance = x["Вид"],
                            ranking_type = ranking_type,
                            competition_coefficient = as.numeric(x["Коэффициент"]))
    })
  }
  else {
    if(ranking_type == "elite") {
      apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
        ranking_scores_elite(results_source = results_source,
                              competition_date = x["Дата"],
                              competition_name = x["Название"],
                              competition_distance = x["Вид"],
                              ranking_type = ranking_type,
                              competition_coefficient = as.numeric(x["Коэффициент"]))
      })
    } else {
      stop("Unsupported ranking type!")
    }
  }
}
