devtools::load_all()


# Simulate some data ----------------------------------------------------------


FOI <- 0.0235
age_groups <- 20
x <- sample(1:50, age_groups, replace = TRUE)
n_j <- x / sum(x)
pop <- 500000

l_lim <- seq(0, 95, 5)
u_lim <- seq(5, 100, 5)
phis <- c(1, 1, 1, 1)


# Run a test ------------------------------------------------------------------


incids <- calculate_incidences(FOI, u_lim, l_lim)

infs <- vapply(incids, incidences_to_numbers, numeric(1), n_j, pop)

R0 <- calculate_R0(FOI, pop, infs, phis)

R0


# Now with real data ----------------------------------------------------------


FOI <- foi[4, "FOI"] # a point in Brazil

FOI_id0 <- foi[4, "ID_0"]

n_j <- age_structure[age_structure$ID_0 == FOI_id0, 2:ncol(age_structure)]

pop <- foi[4, "population"]

incids <- calculate_incidences(FOI, u_lim, l_lim)

infs <- vapply(incids, incidences_to_numbers, numeric(1), n_j, pop)

R0 <- calculate_R0(FOI, pop, infs, phis)

R0


# ------------------------------------


table1 <- readRDS("R0_r_all_squares_2.rds")
table1[table1[, "cell"] == 300000,"1"]

table2 <- readRDS("C_num_all_squares_2.rds")
table2[table2[, "cell"] == 300000,"1"]

table3 <- readRDS("I_num_all_squares_2.rds")
table3[table3[, "cell"] == 300000,"1"]

FOI <- 0.01995189
n_j <- age_structure[age_structure$ID_0 == 185, 2:ncol(age_structure)]
pop <- 432
gamma_1 <- 0.45
rho <- 0.85
gamma_3 <- 0.15

sym_to_asym_ratios <- list(gamma_1, rho, gamma_3, gamma_3)

incids <- calculate_incidences(FOI, u_lim, l_lim)

infs <- vapply(incids, incidences_to_numbers, numeric(1), n_j, pop)

R0 <- calculate_R0(FOI, pop, infs, phis)

R0

cases <- calculate_cases(incids, sym_to_asym_ratios, n_j, pop)

sum(infs)
sum(cases)

### add datasets
dat <- read.csv("All_FOI_estimates_linear_env_var_area.csv")
foi <- dat[,c(1:8)]
foi$population <- dat$population
devtools::use_data(foi, foi, overwrite = TRUE)

write.csv(foi, file.path("vignettes", "foi.csv"),
          row.names = FALSE) # for vignettes

dat2 <- read.csv("country_age_structure.csv")
age_structure <- dat2[setdiff(names(dat2), c("country", "Reference_date", "band_80_99", "band_85_99"))]
devtools::use_data(age_structure, age_structure, overwrite = TRUE)

write.csv(age_structure, file.path("vignettes", "age_structure.csv"),
          row.names = FALSE) # for vignettes

### create vignette

devtools::use_vignette("my-vignette")

