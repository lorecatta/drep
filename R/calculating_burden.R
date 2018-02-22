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
calculate_cases <- function(incidences, sym_to_asym_ratios, n_j, N) {

  ret <- Map("*", incidences, sym_to_asym_ratios)

  vapply(ret, incidences_to_numbers, numeric(1), n_j, N)

}
