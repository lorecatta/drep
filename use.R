devtools::load_all()

## Simulate some data
FOI <- 0.0235
age_groups <- 20
x <- sample(1:50, age_groups, replace = TRUE)
n_j <- x / sum(x)

l_lim <- seq(0, 95, 5)
u_lim <- seq(5, 100, 5)
vec_phis <- c(1, 1, 1, 1)

## Run
R0 <- calculate_R0(FOI, N = 1, n_j, l_lim, u_lim, vec_phis)

R0
