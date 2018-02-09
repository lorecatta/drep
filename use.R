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


incids <- calculate_incidences(FOI, l_lim, u_lim)

infs <- calculate_infections(incids, n_j, pop)

R0 <- calculate_R0(FOI, pop, infs, phis)

R0


# Now with real data ----------------------------------------------------------


FOI <- foi[4, "FOI"] # a point in Brazil

FOI_id0 <- foi[4, "ID_0"]

n_j <- age_structure[age_structure$ID_0 == FOI_id0, 4:ncol(age_structure)]

pop <- foi[4, "population"]

incids <- calculate_incidences(FOI, l_lim, u_lim)

infs <- calculate_infections(incids, n_j, pop)

R0 <- calculate_R0(FOI, pop, infs, phis)

R0


# ------------------------------------


# table1 <- readRDS("R0_r_all_squares_2.rds")
# table1[table1[, "cell"] == 300000,"1"]
#
# table2 <- readRDS("C_num_all_squares_2.rds")
# table2[table2[, "cell"] == 300000,"1"]
#
# table3 <- readRDS("I_num_all_squares_2.rds")
# table3[table3[, "cell"] == 300000,"1"]

FOI <- 0.01995189
n_j <- age_structure[age_structure$ID_0 == 185, 4:ncol(age_structure)]
pop <- 432
gamma_1 <- 0.45
rho <- 0.85
gamma_3 <- 0.15

sym_to_asym_ratios <- list(gamma_1, rho, gamma_3, gamma_3)

incids <- calculate_incidences(FOI, l_lim, u_lim)

infs <- calculate_infections(incids, n_j, pop)

R0 <- calculate_R0(FOI, pop, infs, phis)

R0

cases <- calculate_cases(incids, sym_to_asym_ratios, n_j, pop)

sum(infs)
sum(cases)
