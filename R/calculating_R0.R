#' Calculate the incidence of primary, secondary, tertiary and quaternary dengue
#' infections in a population, for a given force of infection. \emph{Incidence}
#' is the probability that someone has a primary (or secondary, etc..) dengue
#' infection in a particular age group.
#'
#' @title Calculate the incidence of primary, secondary, tertiary and quaternary
#'  dengue infections.
#'
#' @param FOI The force of infection value. Numeric.
#'
#' @param l_lim A numeric vector of the lower age limits of the population age
#'  groups.
#'
#' @param u_lim A numeric vector of the upper age limits of the population age
#'  groups.
#'
#' @return A list containing four different numeric vectors with the incidence
#'  of primary, secondary, tertiary and quaternary infections in each age group.
#'
#' @examples
#' #Assumes population is divided in 5-year age groups
#'
#' age_groups <- 20
#' # a force of infection value
#' a_FOI <- 0.031
#' # the lower limits of the age groups
#' a <- seq(0, 95, length.out = age_groups)
#' # the upper limits of the age groups
#' b <- seq(5, 100, length.out = age_groups)
#' # run
#' calculate_incidences(FOI, a, b)
#'
#' @export
calculate_incidences <- function(FOI, u_lim, l_lim) {

  if(FOI <= 0){

    stop("FOI must be greater than zero")

  }

  get_incidences <- function(infec_prob) {

    (infec_prob / 4) / (u_lim - l_lim)

  }

  p_I1 <- exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim)

  p_I2 <- 4 * (exp(-3 * FOI * l_lim) - exp(-3 * FOI * u_lim)) -
          3 * (exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim))

  p_I3 <- 6 * (exp(-2 * FOI * l_lim) - exp(-2 * FOI * u_lim)) +
          8 * (exp(-3 * FOI * u_lim) - exp(-3 * FOI * l_lim)) +
          3 * (exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim))

  p_I4 <- 4 * (exp(-FOI * l_lim) - exp(-FOI * u_lim) +
               exp(-3 * FOI * l_lim) - exp(-3 * FOI * u_lim)) +
          6 * (exp(-2 * FOI * u_lim) - exp(-2 * FOI * l_lim)) +
              (exp(-4 * FOI * u_lim) - exp(-4 * FOI * l_lim))

  all_probs <- list(p_I1, p_I2, p_I3, p_I4)

  lapply(all_probs, get_incidences)

}


# -----------------------------------------------------------------------------


#' Calculate the \emph{number} of people (infected or displaying disease symptoms)
#' from \emph{incidence} estimates.
#'
#' @title Convert incidences into numbers.
#'
#' @param incidences A numeric vector of the incidence of primary, secondary,
#' tertiary and quaternary dengue infections.
#'
#' @param n_j A numeric vector of the proportions of individuals in each age
#'  group.
#'
#' @param N Population size. Numeric.
#'
#' @return Numeric.
#'
#' @export
incidences_to_numbers <- function(incidences, N, n_j) {

  ret <- incidences * N * n_j * 4

  sum(ret)

}


# -----------------------------------------------------------------------------


#' Calculate the dengue reproduction number using the at-equilibrium number of
#' primary, secondary, tertiary and quaternary infections in a population and
#' their relative infectiousness.
#'
#' @title Calculate the Dengue reproduction number (R0).
#'
#' @param FOI The force of infection value. Numeric.
#'
#' @param N Population size. Numeric.
#'
#' @param n_j A numeric vector of the proportions of individuals in each age
#'  group.
#'
#' @param u_lim A numeric vector of the upper age limits of the population age
#'  groups.
#'
#' @param l_lim A numeric vector of the lower age limits of the population age
#'  groups.
#'
#' @param phis A numeric vector of the relative infectiousness
#'   of each dengue infection.
#'
#' @return Numeric.
#'
#' @export
calculate_R0 <- function(FOI, N, n_j, u_lim, l_lim, phis) {

  incids <- calculate_incidences(FOI, u_lim, l_lim)

  infs <- vapply(incids, incidences_to_numbers, numeric(1), N, n_j)

  FOI * N * 4 / (sum(infs * phis))

}


# -----------------------------------------------------------------------------


#' Wrapper for repeating calculation of the dengue reproduction number over
#' multiple force of infection values.
#'
#' @title Calculate R0 for multiple force of infection values.
#'
#' @param foi_data A matrix with three columns. One contains the GADM adm 0
#' unique identifier for the country of the force of infection data point. One
#' contains the force of infection estimates. One contains the values of population
#' size for the location to which the force of infection refers.
#'
#' @param age_data A matrix with the age structure of the different force of
#'  infections in \code{foi_data}. Each row refers to an individual force of
#'  infection estimate. The first column is the GADM adm 0 unique identifier for
#'  the country of the force of infection data point. The remaining columns
#'  contain the proportions of individuals in each age group.
#'
#' @param id_0_field A character string of the name of the column where the
#'  GADM adm 0 unique identifier for the country is stored. It has to be the same
#'  in the foi_data and the age_data datasets.
#'
#' @param foi_field A character string of the force of infection column name.
#'
#' @param pop_field A character string of the population size column name.
#'
#' @param u_lim A numeric vector of the upper age limits of the population age
#'  groups.
#'
#' @param l_lim A numeric vector of the lower age limits of the population age
#'  groups.
#'
#' @param phis A numeric vector of the relative infectiousness of each dengue
#'  infection.
#'
#' @return A numeric vector.
#'
#' @export
calculate_R0_foi_bulk <- function(foi_data,
                                  age_data,
                                  id_0_field,
                                  foi_field,
                                  pop_field,
                                  u_lim,
                                  l_lim,
                                  phis) {

  wrapper <- function(i) {

    n_j <- age_data[age_data[, id_0_field] == foi_data[i, id_0_field], 2:ncol(age_data)]

    FOI <- foi_data[i, foi_field]

    pop <- foi_data[i, pop_field]

    calculate_R0(FOI, pop, n_j, u_lim, l_lim, phis)

  }

  n <- nrow(foi_data)

  vapply(seq_len(n), wrapper, numeric(1))

}


# -----------------------------------------------------------------------------


#' Calculate the infectiousness of primary, secondary, tertiary and quaternary
#' assuming that symptomatic infections are twice as infecious as asymptomatic
#' ones. Infectiousness of secondary infectious is 1.
#'
#' @title Calculate infectiousness when symptomatic infections are twice as
#'  infectious as asymptomatic ones.
#'
#' @param w_1 proportions of primary infections which are symptomatic.
#'
#' @param w_2 proportions of secondary infections which are symptomatic.
#'
#' @param w_3 proportions of tertiary and quaternary infections which are symptomatic.
#'
#' @return Numeric.
#'
#' @export
calculate_infectiousness_sym_2x_asym <- function(w_1, w_2, w_3) {

  phi_2 <- 1
  phi_1 <- (w_1 * 2 + (1 - w_1)) / (w_2 * 2 + (1 - w_2))
  phi_3 <- phi_4 <- (w_3 * 2 + (1 - w_3)) / (w_2 * 2 + (1 - w_2))

  c(phi_1, phi_2, phi_3, phi_4)
}


# -----------------------------------------------------------------------------


#' Wrapper for repeating calculation of the dengue reproduction number over
#' multiple infectiousness combinations.
#'
#' @title Calculate R0 for multiple infectiousness combinations.
#'
#' @param phi_combs A list with the different combinations of infectiousness
#' of the four dengue infection.
#'
#' @param ... Additional arguments to pass to \code{calculate_R0()}.
#'
#' @return A numeric vector.
#'
#' @export
calculate_R0_infectiouss_bulk <- function(phi_combs, FOI, N, n_j, u_lim, l_lim) {

  wrapper <- function(i) {

    calculate_R0(FOI = FOI,
                 N = N,
                 n_j = n_j,
                 u_lim = u_lim,
                 l_lim = l_lim,
                 phis = i)

  }

  vapply(phi_combs, wrapper, numeric(1))

}


# -----------------------------------------------------------------------------


#' Wrapper for repeating calculation of the dengue reproduction number over
#' multiple force of infection values and combination of infectiousness.
#'
#' @title Calculate R0 for multiple force of infection values and infectiousness
#'  combinations.
#'
#' @param foi_data A matrix with the force of infection (FOI) estimates in one
#' column. The other columns contain other attributes related to the location
#' of the force of infection data point (e.g. longitude, latitude, population etc.).
#'
#' @param age_data A matrix with the age structure of the different force of
#'  infections in \code{foi_data}. Each row refers to an individual force of
#'  infection estimate. The first column is the GADM adm 0 unique identifier for
#'  the country of the force of infection data point. The remaining columns
#'  contain the proportions of individuals in each age group.
#'
#' @param id_0_field A character string of the name of the column where the
#'  GADM adm 0 unique identifier for the country is stored. It has to be the same
#'  in the foi_data and the age_data datasets.
#'
#' @param foi_field A character string of the force of infection column name.
#'
#' @param pop_field A character string of the population size column name.
#'
#' @param u_lim A numeric vector of the upper age limits of the population age
#'  groups.
#'
#' @param l_lim A numeric vector of the lower age limits of the population age
#'  groups.
#'
#' @param phi_combs A list with the different combinations of infectiousness
#' of the four dengue infection.
#'
#' @return A \emph{n} x \emph{m} matrix where \emph{n} is the number of force
#' of infection estimates and emph{m} is the number of infectiousness combinations.
#'
#' @export
calculate_R0_foi_and_infectiouss_bulk <- function(foi_data,
                                                  phi_combs,
                                                  age_data,
                                                  id_0_field,
                                                  foi_field,
                                                  pop_field,
                                                  u_lim,
                                                  l_lim) {

  n <- nrow(foi_data)

  wrapper_1 <- function(i, phis) {

    n_j <- age_data[age_data[, id_0_field] == foi_data[i, id_0_field], 2:ncol(age_data)]

    FOI <- foi_data[i, foi_field]

    pop <- foi_data[i, pop_field]

    calculate_R0(FOI, pop, n_j, u_lim, l_lim, phis)

  }

  wrapper_2 <- function(j) {

    phis <- j

    vapply(seq_len(n), wrapper_1, numeric(1), phis)

  }

  vapply(phi_combs, wrapper_2, numeric(n))

}
