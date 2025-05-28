## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center"
)

## ----include=TRUE, fig.align="center", fig.cap=c("A small DAG with four nodes"), echo=FALSE, out.width=600----
knitr::include_graphics("./images_v_sim_from_dag/simple_dag.png")

## -----------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(simDAG)

dag <- empty_dag()

## -----------------------------------------------------------------------------
dag <- dag + 
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5)

## -----------------------------------------------------------------------------
dag <- dag +
  node("bmi", type="gaussian", parents=c("sex", "age"), betas=c(1.1, 0.4),
       intercept=12, error=2) +
  node("death", type="binomial", parents=c("age", "bmi"), betas=c(0.1, 0.3),
       intercept=-15)

## ----fig.width=6, fig.height=5------------------------------------------------
plot(dag)

## -----------------------------------------------------------------------------
summary(dag)

## -----------------------------------------------------------------------------
set.seed(42)
sim_dat <- sim_from_dag(dag=dag, n_sim=10000)

## -----------------------------------------------------------------------------
head(sim_dat, 5)

## ----fig.width=7, fig.height=5------------------------------------------------
hist(sim_dat$age)

## -----------------------------------------------------------------------------
table(sim_dat$sex)

## -----------------------------------------------------------------------------
mod_bmi <- glm(bmi ~ age + sex, data=sim_dat, family="gaussian")
summary(mod_bmi)

## -----------------------------------------------------------------------------
mod_death <- glm(death ~ age + bmi, data=sim_dat, family="binomial")
summary(mod_death)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("age", type="rnorm") +
  node("sex", type="rbernoulli") +
  node("bmi", type="gaussian", parents=c("sex", "age")) +
  node("death", type="binomial", parents=c("age", "bmi"))

## -----------------------------------------------------------------------------
est_dag <- dag_from_data(dag=dag, data=sim_dat)

## -----------------------------------------------------------------------------
sim_dat2 <- sim_from_dag(dag=est_dag$dag, n_sim=10000)

## ----include=TRUE, fig.align="center", fig.cap=c("A small DAG with four nodes"), echo=FALSE, out.width=600----
knitr::include_graphics("./images_v_sim_from_dag/time_dep_dag.png")

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("A", type="rnorm", mean=50, sd=4) +
  node("B", type="rbernoulli", p=0.5) +
  node("C_1", type="gaussian", formula= ~ 12 + B*1.1 + A*0.4, error=2) +
  node("D_1", type="binomial", formula= ~ -15 + A*0.1 + B*0.3 + C_1*0.1) +
  node("C_2", type="gaussian", formula= ~ 0 + C_1*1.1 + D_1*1, error=2) +
  node("D_2", type="binomial", formula= ~ -15 + D_1*10 + C_2*0.1)

sim_dat <- sim_from_dag(dag=dag, n_sim=10000)

