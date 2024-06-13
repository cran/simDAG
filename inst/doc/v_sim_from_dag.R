## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center"
)

## ---- include=TRUE, fig.align="center", fig.cap=c("A small DAG with four nodes"), echo=FALSE, out.width=400----
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

## ----fig.width=7, fig.height=6------------------------------------------------
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

## ---- include=TRUE, fig.align="center", fig.cap=c("A small DAG with four nodes"), echo=FALSE, out.width=700----
knitr::include_graphics("./images_v_sim_from_dag/time_dep_dag.png")

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi_t1", type="gaussian", betas=c(1.1, 0.4), parents=c("sex", "age"),
       intercept=12, error=2) +
  node("death_t1", type="binomial", parents=c("age", "sex", "bmi_t1"),
       betas=c(0.1, 0.3, 0.1), intercept=-15) +
  node("bmi_t2", type="gaussian", parents="bmi_t1", betas=c(1.1), intercept=0,
       error=2) +
  node("death_t2", type="binomial", betas=c(0.1, 0.3),
       parents=c("age", "bmi_t2"), intercept=-15)

sim_dat <- sim_from_dag(dag=dag, n_sim=10000)

