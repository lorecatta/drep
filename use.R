devtools::load_all()

# load packages

#library(dplyr)


# Define parameters -----------------------------------------------------------


l_lim <- seq(0, 95, 5)
u_lim <- seq(5, 100, 5)
phis <- c(1, 1, 1, 1)



# Simulate some data ----------------------------------------------------------


FOI <- 0.0235
age_groups <- 20
x <- sample(1:50, age_groups, replace = TRUE)
n_j <- x / sum(x)
pop <- 500000


# Run a test ------------------------------------------------------------------


R0 <- calculate_R0(FOI, pop, n_j, u_lim, l_lim, phis)

R0


# Now with real data ----------------------------------------------------------


FOI <- foi[4, "FOI"] # a point in Brazil

FOI_id0 <- foi[4, "ID_0"]

n_j <- age_structure[age_structure$ID_0 == FOI_id0, 2:ncol(age_structure)]

pop <- foi[4, "population"]

R0 <- calculate_R0(FOI, pop, n_j, u_lim, l_lim, phis)

R0


# with multiple FOI values ----------------------------------------------------


# FOI equals zero is not allowed in R0 calculation
foi_pos <- foi[foi[, "type"] != "pseudoAbsence", ]

all_FOI_R0 <- calculate_R0_foi_bulk(foi_data = foi_pos,
                                    age_data = age_structure,
                                    id_0_field = "ID_0",
                                    foi_field = "FOI",
                                    pop_field = "population",
                                    u_lim = u_lim,
                                    l_lim = l_lim,
                                    phis = phis)

all_FOI_R0


# with multiple combinations of infectiousness --------------------------------


w_1 <- 0.45
w_2 <- 0.85
w_3 <- 0.15

sym_2x_asym_comb <- calculate_infectiousness_sym_2x_asym(w_1, w_2, w_3)

infectiousness_combs <- list(c(1, 1, 0, 0),
                             c(1, 1, 1, 1),
                             sym_2x_asym_comb)

all_phis_R0 <- calculate_R0_infectiouss_bulk(phi_combs = infectiousness_combs,
                                             FOI = FOI,
                                             N = pop,
                                             n_j = n_j,
                                             u_lim = u_lim,
                                             l_lim = l_lim)

all_phis_R0


# with multiple FOI AND combinations of infectiousness ------------------------


all_phis_and_FOI_R0 <- calculate_R0_foi_and_infectiouss_bulk(foi_data = foi_pos,
                                                             phi_combs = infectiousness_combs,
                                                             age_data = age_structure,
                                                             id_0_field = "ID_0",
                                                             foi_field = "FOI",
                                                             pop_field = "population",
                                                             u_lim = u_lim,
                                                             l_lim = l_lim)

all_phis_and_FOI_R0




# calculate cases -------------------------------------------------------------


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
devtools::build_vignettes()
