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

