## code to prepare `DATASET` dataset goes here

foi <- read.csv(file.path("data-raw", "foi.csv"))

age_structure <- read.csv(file.path("data-raw", "age_structure.csv"))

usethis::use_data(foi)

usethis::use_data(age_structure)
