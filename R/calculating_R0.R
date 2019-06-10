
# -----------------------------------------------------------------------------

# calculate_incidences

#' \code{calculate_incidences} calculates the incidence of primary, secondary,
#' tertiary and quaternary dengue infections in a population, for a given
#' force of infection. \emph{Incidence} is the probability that someone has a
#' primary (or secondary, etc..) dengue infection in a particular age group.
#'
#' @title Calculate the incidence of primary, secondary, tertiary and quaternary
#'  dengue infections.
#'
#' @param FOI The force of infection value. Numeric.
#'
#' @param u_lim A numeric vector of the upper age limits of the population age
#'  groups.
#'
#' @param l_lim A numeric vector of the lower age limits of the population age
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

  if (FOI <= 0) {
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

# incidences_to_numbers

#' \code{incidences_to_numbers} calculates the \emph{number} of people
#' (infected or displaying disease symptoms) from \emph{incidence} estimates.
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

# calculate_R0

#' \code{calculate_R0} calculates the dengue reproduction number using the
#' at-equilibrium number of primary, secondary, tertiary and quaternary infections
#' in a population and their relative infectiousness.
#'
#' @title Calculate the Dengue reproduction number (\eqn{R_0})
#'
#' @inheritParams calculate_incidences
#'
#' @inheritParams incidences_to_numbers
#'
#' @param phis A numeric vector of the relative infectiousness
#'   of each of the four dengue infections.
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

# calculate_infectiousness_sym_2x_asym

#' \code{calculate_infectiousness_sym_2x_asym} calculates the infectiousness of
#' primary, secondary, tertiary and quaternary assuming that symptomatic infections
#' are twice as infecious as asymptomatic ones.
#' Infectiousness of secondary infectious is 1.
#'
#' @title Calculate infectiousness when symptomatic infections are twice as
#'  infectious as asymptomatic ones.
#'
#' @param prop_sym_parms numeric vector of proportions of primary, secondary and
#' tertiary/quaternary infections which are symptomatic.
#'
#' @return Numeric.
#'
#' @export


calculate_infectiousness_sym_2x_asym <- function(prop_sym_parms) {

  w_1 <- prop_sym_parms[1]
  w_2 <- prop_sym_parms[2]
  w_3 <- prop_sym_parms[3]

  phi_2 <- 1
  phi_1 <- (w_1 * 2 + (1 - w_1)) / (w_2 * 2 + (1 - w_2))
  phi_3 <- (w_3 * 2 + (1 - w_3)) / (w_2 * 2 + (1 - w_2))

  c(phi_1, phi_2, phi_3, phi_3)

}
