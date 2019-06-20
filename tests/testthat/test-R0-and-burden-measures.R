test_that("R0 and burden outputs are correct", {

  correct_outputs <- read.table(file.path("test_data", "R0_and_burden_calculation_test.txt"), header = TRUE)

  prop_sym_parms <- c(0.45, 0.85, 0.15, 0.15)

  hosp_weights <- c(0.04, 0.1, 0.04, 0.04)

  R0_1_phis <- c(1, 1, 0, 0)
  R0_2_phis <- c(1, 1, 1, 1)
  R0_3_phis <- calculate_infectiousness_sym_2x_asym(prop_sym_parms)

  age_band_tgs <- grep("band", names(age_structure), value = TRUE)
  age_band_bnds <- get_age_band_bounds(age_band_tgs)
  l_lim <- age_band_bnds[, 1]
  u_lim <- age_band_bnds[, 2]

  R0_and_burden <- purrr::pmap(foi[, c("FOI", "population", "ID_0")],
                               function(FOI, population, ID_0){

                                 n_j <- age_structure[which(age_structure[, "ID_0"] == ID_0), 2:ncol(age_structure)]

                                 R0_1 <- calculate_R0(FOI = FOI,
                                                      n_j = n_j,
                                                      u_lim = u_lim,
                                                      l_lim = l_lim,
                                                      phis = R0_1_phis)

                                 R0_2 <- calculate_R0(FOI = FOI,
                                                      n_j = n_j,
                                                      u_lim = u_lim,
                                                      l_lim = l_lim,
                                                      phis = R0_2_phis)

                                 R0_3 <- calculate_R0(FOI = FOI,
                                                      n_j = n_j,
                                                      u_lim = u_lim,
                                                      l_lim = l_lim,
                                                      phis = R0_3_phis)

                                 no_infections <- calculate_infections(FOI = FOI,
                                                                          n_j = n_j,
                                                                          u_lim = u_lim,
                                                                          l_lim = l_lim,
                                                                          N = population)

                                 no_cases <- calculate_cases(FOI = FOI,
                                                                n_j = n_j,
                                                                u_lim = u_lim,
                                                                l_lim = l_lim,
                                                                weights_vec = prop_sym_parms,
                                                                N = population)

                                 no_hosp_cases <- calculate_hosp_cases(FOI = FOI,
                                                                       n_j = n_j,
                                                                       u_lim = u_lim,
                                                                       l_lim = l_lim,
                                                                       parms = hosp_weights,
                                                                       weights_vec = prop_sym_parms,
                                                                       N = population)

                                 c(R0_1 = R0_1,
                                   R0_2 = R0_2,
                                   R0_3 = R0_3,
                                   infections = no_infections,
                                   cases = no_cases,
                                   hosp = no_hosp_cases)

                               })

  R0_and_burden_df <- do.call("rbind", R0_and_burden)

  expect_equal(R0_and_burden_df[, "R0_1"], correct_outputs$R0_1, tolerance = 1e-10)
  expect_equal(R0_and_burden_df[, "R0_2"], correct_outputs$R0_2, tolerance = 1e-10)
  expect_equal(R0_and_burden_df[, "R0_3"], correct_outputs$R0_3, tolerance = 1e-10)
  expect_equal(R0_and_burden_df[, "infections"], correct_outputs$infections, tolerance = 1e-10)
  expect_equal(R0_and_burden_df[, "cases"], correct_outputs$cases, tolerance = 1e-10)
  expect_equal(R0_and_burden_df[, "hosp"], correct_outputs$hosp, tolerance = 1e-10)

})
