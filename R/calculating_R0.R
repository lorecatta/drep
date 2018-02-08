#' Calculate the total number of individuals (in a population) infected
#' with dengue for their first, second, third or fourth time,
#' during their lifetime.
#'
#' @title Calculate probability of dengue infections in a population.
#'
#' @param FOI The force of infection value. Numeric.
#'
#' @param l_lim A numeric vector of the lower age limits of the population age
#'  groups.
#'
#' @param u_lim A numeric vector of the upper age limits of the population age
#'  groups.
#'
#' @param n_j A numeric vector of the proportions of individuals in each age
#'  group
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
#' # simulate age structure
#' x <- sample(1:50, age_groups, replace = TRUE)
#' n_j <- x / sum(x)
#' # run
#' calculate_infections(FOI, a, b, n_j)
#'
#' @export
calculate_infections <- function(FOI, l_lim, u_lim, n_j) {

  repeat_routine <- function(infec_prob, u_lim, l_lim, n_j) {

    infec_incid <- (infec_prob / 4) / (u_lim - l_lim)
    infec_num_j <- infec_incid * n_j
    sum(infec_num_j)

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

  ret <- list(p_I1, p_I2, p_I3, p_I4)

  vapply(ret,
         repeat_routine,
         numeric(1),
         u_lim,
         l_lim,
         n_j)

}

#' Calculate the dengue reproduction number (R0) using the
#' at-equilibrium total number of primary, secondary, tertiary and
#' quaternary infections in a population and their realtive infectiousness.
#'
#' @title Calculate dengue reproduction number (R0).
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
calculate_R0 <- function(FOI, infections, phis) {

  FOI / (sum(infections * phis))

}

# calculate_primary_infection_prob <- function(FOI, l_lim, u_lim) {
#
#   exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim)
#
# }
#
# calculate_secondary_infection_prob <- function(FOI, l_lim, u_lim) {
#
#   4 * (exp(-3 * FOI * l_lim) - exp(-3 * FOI * u_lim)) -
#     3 * (exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim))
#
# }
#
# calculate_tertiary_infection_prob <- function(FOI, l_lim, u_lim) {
#
#   6 * (exp(-2 * FOI * l_lim) - exp(-2 * FOI * u_lim)) +
#     8 * (exp(-3 * FOI * u_lim) - exp(-3 * FOI * l_lim)) +
#     3 * (exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim))
#
# }
#
# calculate_quaternary_infection_prob <- function(FOI, l_lim, u_lim) {
#
#   4 * (exp(-FOI * l_lim) - exp(-FOI * u_lim) +
#          exp(-3 * FOI * l_lim) - exp(-3 * FOI * u_lim)) +
#     6 * (exp(-2 * FOI * u_lim) - exp(-2 * FOI * l_lim)) +
#     (exp(-4 * FOI * u_lim) - exp(-4 * FOI * l_lim))
#
# }
#
# calculate_infection_incidence <- function(infect_prob, u_lim, l_lim) {
#
#   (infect_prob / 4) / (u_lim - l_lim)
#
# }
#
# calculate_infection_number <- function(incidence, n_j) {
#
#   incidence * n_j
#
# }
#
# calculate_R0 <- function(FOI, n_j, l_lim, u_lim, vec_phis) {
#
#   prob_funs <- list("calculate_primary_infection_prob",
#                     "calculate_secondary_infection_prob",
#                     "calculate_tertiary_infection_prob",
#                     "calculate_quaternary_infection_prob")
#
#   infec_probs <- lapply(prob_funs, do.call, list(FOI, l_lim, u_lim))
#
#   infec_incid <- lapply(infec_probs,
#                         calculate_infection_incidence,
#                         u_lim,
#                         l_lim)
#
#   infec_num_j <- lapply(infec_incid, calculate_infection_number, n_j)
#
#   infec_num <- vapply(infec_num_j, sum, numeric(1))
#
#   N <- 1
#
#   FOI * N / (sum(infec_num * vec_phis))
#
# }
