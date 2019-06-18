#' Geolocated force of infection estimates of dengue
#'
#' A dataset containing the values and other attributes of > 580
#' force of infection estimates of dengue. The variables are as follows:
#'
#' @format A data frame with 693 rows and 11 variables:
#' \describe{
#'    \item{data_id}{unique identifier}
#'    \item{type}{type of study where the force of infection estimate was sourced from (serology, caseReport, pseudoAbsence)}
#'    \item{date}{date of the study}
#'    \item{longitude}{longitude of the location of the study}
#'    \item{latitude}{latitude of the location of the study}
#'    \item{country}{country where the study was carried out}
#'    \item{ISO}{3-digits country code}
#'    \item{ID_0}{GADM administrative unit level 0 code}
#'    \item{ID_1}{GADM administrative unit level 1 code}
#'    \item{FOI}{force of infection estimate (0.00000000--0.06159744)}
#'    \item{R0_1}{reproduction number assuming onlyt primary and secondayr infections are asymptomatic}
#'    \item{population}{size of the human population in level 1 adm unit}
#'  }
#' @references \url{https://mrcdata.dide.ic.ac.uk/_dengue/dengue.php}
"foi"


#' Country age structures
#'
#' A dataset containing the proportions of individuals in each of twenty 5-year
#' age groups. The variables are as follows:
#'
#' @format A data with frame with 200 rows and 21 variables:
#' \describe{
#'    \item{ID_0}{GADM administrative unit level 0 code}
#'    \item{band_0_4}{proportion of individuals of age 0-4}
#'    \item{band_5_9}{proportion of individuals of age 5-9}
#'    \item{band_10_14}{proportion of individuals of age 10-14}
#'    \item{band_15_19}{proportion of individuals of age 15-19}
#'    \item{band_20_24}{proportion of individuals of age 20-24}
#'    \item{band_25_29}{proportion of individuals of age 25-20}
#'    \item{band_31_34}{proportion of individuals of age 31-34}
#'    \item{band_35_39}{proportion of individuals of age 35-39}
#'    \item{band_40_44}{proportion of individuals of age 40-44}
#'    \item{band_45_49}{proportion of individuals of age 45-49}
#'    \item{band_50_54}{proportion of individuals of age 50-54}
#'    \item{band_55_59}{proportion of individuals of age 55-59}
#'    \item{band_60_64}{proportion of individuals of age 60-64}
#'    \item{band_65_69}{proportion of individuals of age 65-69}
#'    \item{band_70_74}{proportion of individuals of age 70-74}
#'    \item{band_75_79}{proportion of individuals of age 75-79}
#'    \item{band_80_84}{proportion of individuals of age 80-84}
#'    \item{band_85_89}{proportion of individuals of age 85-89}
#'    \item{band_90_94}{proportion of individuals of age 90-94}
#'    \item{band_95_99}{proportion of individuals of age 95-99}
#' }
#' @references \url{https://population.un.org/wpp/Download/Standard/Population/}
"age_structure"
