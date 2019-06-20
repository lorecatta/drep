test_that("R0_1 calculation is correct", {

  correct_outputs <- read.table(file.path("test_data", "R0_and_burden_calculation_test.txt"), header = TRUE)

  phis <- c(1, 1, 0, 0)

  age_band_tgs <- grep("band", names(age_structure), value = TRUE)
  age_band_bnds <- get_age_band_bounds(age_band_tgs)
  l_lim <- age_band_bnds[, 1]
  u_lim <- age_band_bnds[, 2]

  R0_1 <- purrr::pmap(foi[, c("FOI", "population", "ID_0")],
                      function(FOI, population, ID_0){

                        n_j <- age_structure[which(age_structure[, "ID_0"] == ID_0), 2:ncol(age_structure)]

                        calculate_R0(FOI = FOI,
                                     n_j = n_j,
                                     u_lim = u_lim,
                                     l_lim = l_lim,
                                     phis = phis)

                      })

  R0_1_vec <- unlist(R0_1)

  expect_equal(R0_1_vec, correct_outputs$R0_1, tolerance = 1e-10)

})
