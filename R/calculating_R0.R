
# -----------------------------------------------------------------------------

# calculate_incidences

#' \code{calculate_incidences} calculates the per-capita incidence rates of primary,
#' secondary, tertiary and quaternary dengue infections in individuals of a
#' particular age group, given a force of infection.
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
#' #Assumes human population divided in 5-year age groups and 20 age groups
#'
#' age_groups <- 20
#' # a force of infection value
#' a_FOI_value <- 0.031
#' # the lower limits of the age groups
#' a <- seq(0, 95, length.out = age_groups)
#' # the upper limits of the age groups
#' b <- seq(5, 100, length.out = age_groups)
#' # run
#' calculate_incidences(a_FOI_value, a, b)
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

#' \code{incidences_to_numbers} calculates the \emph{numbers} of people from different
#' age groups experiencing their primary, secondary, tertiary or quaternary
#' dengue infection, from estimates of the incidence \emph{rates} of primary, secondary,
#' tertiary or quaternary infections in each age group.
#'
#' @title Convert incidences into numbers.
#'
#' @param incidences A numeric vector of the incidence rates of primary, secondary,
#' tertiary or quaternary dengue infections in each age group.
#'
#' @param n_j A numeric vector of the proportion of individuals in each age group
#' in the human population.
#'
#' @return Numeric.
#'
#' @export


incidences_to_numbers <- function(incidences, n_j) {

  incidences * n_j

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
#' @param phis A numeric vector of length = 4 of the relative infectiousness
#'   of primary, secondary, tertiary and quaternary dengue infections.
#'
#' @return Numeric.
#'
#' @export


calculate_R0 <- function(FOI, n_j, u_lim, l_lim, phis) {

  incids <- calculate_incidences(FOI, u_lim, l_lim)

  infection_numbers_j <- lapply(incids, incidences_to_numbers, n_j)

  total_infection_numbers <- vapply(infection_numbers_j, sum, numeric(1))

  N <- 1 # pop size

  FOI * N / (sum(total_infection_numbers * phis))

}


# -----------------------------------------------------------------------------

# calculate_infectiousness_sym_2x_asym

#' \code{calculate_infectiousness_sym_2x_asym} calculates the infectiousness values
#' of primary, secondary, tertiary and quaternary assuming that symptomatic
#' infections are twice as infecious as asymptomatic ones. The infectiousness of
#' secondary infectious equals 1.
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
