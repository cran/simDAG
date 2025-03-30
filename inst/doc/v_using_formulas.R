## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center"
)

## -----------------------------------------------------------------------------
library(simDAG)

dag <- empty_dag() +
  node("A", type="rnorm", mean=0, sd=1) +
  node("B", type="rbernoulli", p=0.5, output="numeric") +
  node("C", type="rcategorical", probs=c(0.3, 0.2, 0.5),
       output="factor", labels=c("low", "medium", "high"))

## -----------------------------------------------------------------------------
set.seed(23143)

dat <- sim_from_dag(dag, n_sim=10)
head(dat)

## -----------------------------------------------------------------------------
dag_without_formula <- dag +
  node("D", type="gaussian", parents=c("A", "B"), betas=c(0.4, -2),
       intercept=-8, error=1.5)

## -----------------------------------------------------------------------------
dag_with_formula <- dag +
  node("D", type="gaussian", formula= ~ -8 + A*0.4 + B*-2, error=1.5)

## -----------------------------------------------------------------------------
set.seed(34)
dat1 <- sim_from_dag(dag_without_formula, n_sim=100)

set.seed(34)
dat2 <- sim_from_dag(dag_with_formula, n_sim=100)

all.equal(dat1, dat2)

## -----------------------------------------------------------------------------
dag2 <- dag +
  node("D", type="gaussian", error=1.5,
       formula=~ -8 + A*0.4 + B*-2 + Cmedium*-1 + Chigh*-3,
       parents=c("A", "B", "C"))

## -----------------------------------------------------------------------------
dag3 <- dag +
  node("D", type="gaussian", formula= ~ -8 + A*0.4 + B*-2 + A:B*-5, error=1.5)

## -----------------------------------------------------------------------------
dag4 <- dag +
  node("D", type="gaussian", error=1.5,
       formula=~ -8 + A*0.4 + B*-2 + Cmedium*-1 + Chigh*-3 + A:Cmedium*0.3 + 
         A:Chigh*10,
       parents=c("A", "B", "C"))

## -----------------------------------------------------------------------------
dag_with_formula <- dag +
  node("D", type="gaussian", formula= ~ -8 + A*0.4 + I(A^2)*0.02 + B*-2,
       error=1.5)

## -----------------------------------------------------------------------------
dag_with_fun <- dag +
  node("D", type="binomial", formula= ~ -3 + A*log(0.5) + B*0.2)

## -----------------------------------------------------------------------------
dag_with_fun <- dag +
  node("this-var", type="binomial", formula= ~ -3 + A*log(0.5) + B*0.2) +
  node("D", type="binomial", formula= ~ 5 + `this-var`*0.3)

## -----------------------------------------------------------------------------
dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="gaussian", formula= ~ -2 + Age*1.2 + (1|School),
       error=1, var_corr=0.3)

## -----------------------------------------------------------------------------
var_corr <- matrix(c(0.5, 0.05, 0.05, 0.1), 2)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="gaussian", formula= ~ -2 + Age*1.2 + (Age|School),
       error=1, var_corr=var_corr)

## -----------------------------------------------------------------------------
beta_coef <- log(0.5)

dag_with_external <- dag +
  node("D", type="binomial", formula= ~ -3 + A*eval(beta_coef) + B*0.2)

## -----------------------------------------------------------------------------
beta_coef <- log(0.5)

form_D <- paste("~ -3 + A*", beta_coef, "+ B*0.2")

dag_with_external <- dag +
  node("D", type="binomial", formula=form_D)

## -----------------------------------------------------------------------------
set.seed(123)

custom_fun <- function(data, parents, betas, intercept) {
  print(head(data))
  print(parents)
  print(betas)
  print(intercept)
  return(rep(1, nrow(data)))
}

dag_custom <- empty_dag() +
  node(c("A", "B"), type="rnorm", mean=0, sd=1) +
  node("C", type="rcategorical", probs=c(0.5, 0.5), labels=c("lev1", "lev2"))

## -----------------------------------------------------------------------------
dag_custom2 <- dag_custom +
  node("Y", type=custom_fun, formula= ~ -5 + A*2 + B*-0.4)

data <- sim_from_dag(dag_custom2, n_sim=10)

## -----------------------------------------------------------------------------
dag_custom2 <- dag_custom +
  node("Y", type=custom_fun, formula= ~ -5 + A*2 + B*-0.4 + A:B*0.1 + 
         I(A^2)*-0.1 + Clev2*0.2)

data <- sim_from_dag(dag_custom2, n_sim=10)

## -----------------------------------------------------------------------------
custom_linreg <- function(data, parents, betas, intercept) {
  intercept + rowSums(mapply("*", data, betas)) +
    rnorm(n=nrow(data), mean=0, sd=1)
}

dag_custom3 <- dag_custom +
  node("Y", type=custom_linreg, formula= ~ -5 + A*2 + B*-0.4 + A:B*0.1 + 
         I(A^2)*-0.1 + Clev2*0.2)

data <- sim_from_dag(dag_custom3, n_sim=100)
head(data)

## -----------------------------------------------------------------------------
# same as before, but without an intercept (same as setting intercept=0)
custom_linreg2 <- function(data, parents, betas) {
  rowSums(mapply("*", data, betas)) +
    rnorm(n=nrow(data), mean=0, sd=1)
}

dag_custom4 <- dag_custom +
  node("Y", type=custom_linreg2, formula= ~ A*2 + B*-0.4 + A:B*0.1 + 
         I(A^2)*-0.1 + Clev2*0.2)

data <- sim_from_dag(dag_custom4, n_sim=100)
head(data)

