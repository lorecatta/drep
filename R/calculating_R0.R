calculate_primary_infection_prob <- function(FOI, l_lim, u_lim) {

  exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim)

}

calculate_secondary_infection_prob <- function(FOI, l_lim, u_lim) {

  4 * (exp(-3 * FOI * l_lim) - exp(-3 * FOI * u_lim)) -
    3 * (exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim))

}

calculate_tertiary_infection_prob <- function(FOI, l_lim, u_lim) {

  6 * (exp(-2 * FOI * l_lim) - exp(-2 * FOI * u_lim)) +
    8 * (exp(-3 * FOI * u_lim) - exp(-3 * FOI * l_lim)) +
    3 * (exp(-4 * FOI * l_lim) - exp(-4 * FOI * u_lim))

}

calculate_quaternary_infection_prob <- function(FOI, l_lim, u_lim) {

  4 * (exp(-FOI * l_lim) - exp(-FOI * u_lim) +
         exp(-3 * FOI * l_lim) - exp(-3 * FOI * u_lim)) +
    6 * (exp(-2 * FOI * u_lim) - exp(-2 * FOI * l_lim)) +
    (exp(-4 * FOI * u_lim) - exp(-4 * FOI * l_lim))

}

calculate_average_infection_prob <- function(infect_prob, u_lim, l_lim) {

  (infect_prob / 4) / (u_lim - l_lim)

}

calculate_case_number <- function(incidence, n_j) {

  incidence * n_j

}

calculate_R0 <- function(FOI, N, n_j, l_lim, u_lim, vec_phis) {

  prob_fun <- list("calculate_primary_infection_prob",
                   "calculate_secondary_infection_prob",
                   "calculate_tertiary_infection_prob",
                   "calculate_quaternary_infection_prob")

  infec_probs <- lapply(prob_fun, do.call, list(FOI, l_lim, u_lim))

  infec_incid <- lapply(infec_probs, calculate_average_infection_prob, u_lim, l_lim)

  infec_num_j <- lapply(infec_incid, calculate_case_number, n_j)

  infec_num <- vapply(infec_num_j, sum, numeric(1))

  FOI * N / (sum(infec_num * vec_phis))

}
