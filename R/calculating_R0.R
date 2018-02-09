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
#' @return Numeric.
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
calculate_incidences <- function(FOI, l_lim, u_lim) {

  get_incidences <- function(infec_prob, u_lim, l_lim) {

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

  lapply(all_probs,
         get_incidences,
         u_lim,
         l_lim)

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
incidences_to_numbers <- function(incidences, n_j, N) {

  if(!is.numeric(n_j)){

    n_j <- as.numeric(n_j)

  }

  incidences * n_j * N * 4

}


# -----------------------------------------------------------------------------


#' Calculate the number of primary, secondary, tertiary and quaternary
#'  dengue infections in a population.
#'
#' @title Calculate the number of primary, secondary, tertiary and quaternary
#'  dengue infections.
#'
#' @param incidences A numeric vector of incidence of primary, secondary,
#' tertiary and quaternary dengue infections.
#'
#' @param n_j A numeric vector of the proportions of individuals in each age
#'  group.
#'
#' @param N Population size. Numeric
#'
#' @return Numeric.
#'
#' @examples
#' incidences <- c(0.0035, 0.002, 0.00123, 0.00569)
#' # simulate age structure
#' x <- sample(1:50, age_groups, replace = TRUE)
#' n_j <- x / sum(x)
#' # run
#' calculate_infections(FOI, a, b, n_j)
#'
#' @export
calculate_infections <- function(incidences, n_j, N) {

  n <- length(n_j)

  infection_numbers_j <- lapply(incidences,
                                incidences_to_numbers,
                                n_j,
                                N)

  vapply(infection_numbers_j,
         sum,
         numeric(1))
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
#' @param infections A numeric vector of the number of primary, secondary,
#'  tertiary and quaternary infections
#'
#' @param phis A numeric vector of the relative infectiousness
#'   of each dengue infection
#'
#' @return Numeric.
#'
#' @examples
#' # a force of infection value
#' a_FOI <- 0.031
#' # a vector of infection numbers
#' vec_infections <- c(10, 3000, 1, 42)
#' a vector of infectiousness weights
#' vec_phis <- c(1, 1, 1, 1)
#' # run
#' calculate_R0(a_Foi, vec_infections, vec_phis)
#'
#' @export
calculate_R0 <- function(FOI, N, infections, phis) {

  FOI * N * 4 / (sum(infections * phis))

}


# -----------------------------------------------------------------------------


#' Calculate the number of primary, secondary, tertiary and quaternary
#'  dengue symptomatic cases in a population.
#'
#' @title Calculate the number of primary, secondary, tertiary and quaternary
#'  dengue cases.
#'
#' @param incidences A numeric vector of incidence of primary, secondary,
#'  tertiary and quaternary dengue infections.
#'
#' @param sym_to_asym_ratios A list of the proportion of symptomatic primary,
#'  secondary, tertiary and quaternary cases (in this order).
#'
#' @param ... Additional arguments to pass to \code{incidence_to_numbers()}
#'
#' @return Numeric.
#'
#' @export
calculate_cases <- function(incidences, sym_to_asym_ratios, ...) {

  ret1 <- Map("*", incidences, sym_to_asym_ratios)

  case_numbers_j <- lapply(ret1,
                           incidences_to_numbers,
                           ...)

  vapply(case_numbers_j,
         sum,
         numeric(1))

}
