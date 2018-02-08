devtools::load_all()

## Simulate some data
FOI <- 0.0235
age_groups <- 20
x <- sample(1:50, age_groups, replace = TRUE)
n_j <- x / sum(x)

l_lim <- seq(0, 95, 5)
u_lim <- seq(5, 100, 5)
phis <- c(1, 1, 1, 1)

## Run test
infs <- calculate_infections(FOI, l_lim, u_lim, n_j)

R0 <- calculate_R0(FOI, infs, phis)

R0

# Now with real data
FOI <- foi[4, "FOI"] # a point in Brazil

FOI_id0 <- foi[4, "ID_0"]

n_j <- age_structure[age_structure$ID_0 == FOI_id0, 4:ncol(age_structure)]

infs <- calculate_infections(FOI, l_lim, u_lim, n_j)

R0 <- calculate_R0(FOI, infs, phis)

foi <- read.csv("All_FOI_estimates_linear_env_var_area.csv")
foi <- foi[,setdiff(names(foi), c("R0_1", "R0_2", "R0_3"))]
devtools::use_data(foi, foi, overwrite = TRUE)
devtools::document()
