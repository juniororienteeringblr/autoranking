# For Linux
# Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
# For Windows
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

source("ranking_scores_youth.R", encoding = "UTF-8")
source("selection_scores_youth.R", encoding = "UTF-8")
source("ranking_scores_master.R", encoding = "UTF-8")
source("ranking_scores_elite.R", encoding = "UTF-8")
source("ranking_scores_junior.R", encoding = "UTF-8")

ranking_type = 'youth' # Available options: "youth", "junior", "master", "elite", "sprint", 'mtbo', 'ski' 'youth_selection'

coefs_comps <- data.frame()

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
              if(ranking_type == "youth_selection") {
                googlesheet_name <- "Youth Selection Starts"
              } else {
                stop("Unsupported rating type!")
              }
            }
          }
        }
      }
    }
  }
}

coefs_comps <- as.data.frame(read_sheet(drive_find(pattern = googlesheet_name,
                                                   type = "spreadsheet"),
                                        sheet = format(Sys.Date(), "%Y")))
coefs_comps$Дата <- as.character(coefs_comps$Дата)

passed_comps <- coefs_comps[!is.na(coefs_comps$`Ссылка на результаты`), ]

if (ranking_type == "youth") {
  apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
    ranking_scores_youth(competition_date = x["Дата"],
                   competition_name = x["Название"],
                   competition_distance = x["Вид"],
                   ranking_type = ranking_type,
                   competition_coefficient = as.numeric(x["Коэффициент"]))
  })
} else {
  if(ranking_type == "master") {
    apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
      ranking_scores_master(competition_date = x["Дата"],
                            competition_name = x["Название"],
                            competition_distance = x["Вид"],
                            ranking_type = ranking_type,
                            competition_coefficient = as.numeric(x["Коэффициент"]))
    })
  } else {
    if ((ranking_type == "elite") | (ranking_type == "sprint") | (ranking_type == "mtbo") | (ranking_type == "ski")) {
      apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
        ranking_scores_elite(competition_date = x["Дата"],
                             competition_name = x["Название"],
                             competition_distance = x["Вид"],
                             ranking_type = ranking_type,
                             competition_coefficient = as.numeric(x["Коэффициент"]))
      })
    } else {
      if  (ranking_type == "junior") {
        apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
          ranking_scores_junior(competition_date = x["Дата"],
                                competition_name = x["Название"],
                                competition_distance = x["Вид"],
                                ranking_type = ranking_type,
                                competition_coefficient = as.numeric(x["Коэффициент"]))
        })
      } else {
        if  (ranking_type == "youth_selection") {
          apply(X = passed_comps, MARGIN = 1, FUN = function(x) {
            selection_scores_youth(competition_date = x["Дата"],
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
  }
}
