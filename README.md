# drep

> Calculation of the Dengue REProduction number from Force of Infection

This package calculates the Dengue Reproduction number, R<sub>0</sub>, from 
Force of Infection using the at-equilibrium number of primary, secondary, tertiary 
and quaternary infections in a population and their relative infectiousness. 
This is done assuming that Dengue transmission is at equilibrium. 

## Installation

You can install the development version of the package using `devtools`. First install `devtools`, if you don't already have it.

```{r include = TRUE, eval = FALSE}
install.packages("devtools")
library(devtools)
```

Then, in a fresh R session, install the `drep` package.

```{r include = TRUE, eval = FALSE}
devtools::install_github("mrc-ide/drep")
library(drep)
```
## Example

Define some parameters

* lower and upper age limits (`l_lim` and `u_lim`) of the country age groups
* relative infectiousness of the four dengue infections (`phis`)

```{r}
l_lim <- seq(0, 95, 5)
u_lim <- seq(5, 100, 5)
phis <- c(1, 1, 1, 1)
```

Simulate some data

* number of age groups in the human population (`age_groups`)
* proportion of individuals in eah age gorup (`n_j`)
* total human population of the country (`pop`)
* the Force of Infection estimate (`FOI`)

```{r}
age_groups <- 20
x <- sample(1:50, age_groups, replace = TRUE)
n_j <- x / sum(x)
pop <- 500000
FOI <- 0.0235
```

Calculate the R<sub>0</sub>

```{r}
R0 <- calculate_R0(FOI, pop, n_j, u_lim, l_lim, phis)

R0
```
