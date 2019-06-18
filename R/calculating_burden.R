
# -----------------------------------------------------------------------------

# calculate_infections

#' \code{calculate_infections} calculates the number of annual dengue infections
#' in a human population, given a force of infection.
#'
#' @title Calculate the number of dengue infections.
#'
#' @inheritParams calculate_R0
#'
#' @param N Size of human population. NUmeric.
#'
#' @return Numeric.
#'
#' @export


calculate_infections <- function(FOI,
                                 n_j,
                                 u_lim,
                                 l_lim,
                                 N){

  incids <- calculate_incidences(FOI, u_lim, l_lim)

  infection_numbers_j <- lapply(incids, incidences_to_numbers, n_j)

  total_infection_number <- vapply(infection_numbers_j, sum, numeric(1))

  sum(total_infection_number) * 4 * N

}


# -----------------------------------------------------------------------------

# calculate_cases

#' \code{calculate_cases} calculates the number of annual dengue symptomatic
#' infections (\emph{cases}) in a human population, given a force of infection.
#'
#' @title Calculate the number of dengue cases.
#'
#' @inheritParams calculate_R0
#'
#' @inheritParams calculate_infections
#'
#' @param weights_vec A numeric vector of the proportions of primary, secondary,
#' tertiary and quaternary infections which are symptomatic.
#'
#' @return Numeric.
#'
#' @export


calculate_cases <- function(FOI,
                            n_j,
                            u_lim,
                            l_lim,
                            weights_vec,
                            N){

  gamma_1 <- weights_vec[1]
  rho <- weights_vec[2]
  gamma_3 <- weights_vec[3]

  incids <- calculate_incidences(FOI, u_lim, l_lim)

  I1_rate <- incids[[1]]
  I2_rate <- incids[[2]]
  I3_rate <- incids[[3]]
  I4_rate <- incids[[4]]

  tot_incid_rate_j <- rho * I2_rate + (gamma_1 * I1_rate) + (gamma_3 * (I3_rate + I4_rate))

  case_number_j <- incidences_to_numbers(tot_incid_rate_j, n_j)

  sum(case_number_j) * 4 * N

}


# -----------------------------------------------------------------------------

# calculate_hosp_cases

#' \code{calculate_hosp_cases} calculates the number of annual dengue symptomatic
#' infections (\emph{cases}) requiring hospitalization in a human population,
#' given a force of infection.
#'
#' @title Calculate the number of dengue hospitalized cases.
#'
#' @inheritParams calculate_R0
#'
#' @inheritParams calculate_infections
#'
#' @inheritParams calculate_cases
#'
#' @param parms A numeric vector of the proportions of primary, secondary and
#' tertiary and quaternary infections requiring hospitalization.
#'
#' @return Numeric.
#'
#' @export


calculate_hosp_cases <- function(FOI,
                                 n_j,
                                 u_lim,
                                 l_lim,
                                 parms,
                                 weights_vec,
                                 N){

  gamma_1 <- weights_vec[1]
  rho <- weights_vec[2]
  gamma_3 <- weights_vec[3]

  Q_1 <- parms[1]
  Q_2 <- parms[2]
  Q_3 <- parms[3]

  incids <- calculate_incidences(FOI, u_lim, l_lim)

  I1_rate <- incids[[1]]
  I2_rate <- incids[[2]]
  I3_rate <- incids[[3]]
  I4_rate <- incids[[4]]

  tot_incid_rate_j <- Q_2 * rho * I2_rate + (Q_1 * gamma_1 * I1_rate) + (gamma_3 * Q_3 * (I3_rate + I4_rate))

  case_number_j <- incidences_to_numbers(tot_incid_rate_j, n_j)

  sum(case_number_j) * 4 * N

}


# -----------------------------------------------------------------------------

# get_age_band_bounds

#' \code{get_age_band_bounds} returns the lower and upper limits of an age group
#' character string of the format e.g. \code{band_X_Y}, where X and Y represent
#' the age limits.
#'
#' @param tags Character string of the age group
#'
#' @return Numeric.
#'
#' @export


get_age_band_bounds <- function(tags) {

  age_band_tags_split <- strsplit(tags, "[^0-9]+")

  age_band_tags_split_num <- lapply(age_band_tags_split, as.numeric)

  age_band_tags_split_num_mat <- do.call("rbind", age_band_tags_split_num)

  age_band_tags_split_num_mat[, 2:3]

}
