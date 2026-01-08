## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>"
)

## ----include=TRUE, fig.align="center", fig.cap=c("A generalized flow-chart of the discrete-time simulation approach"), echo=FALSE, out.width=500----
knitr::include_graphics("./images_v_sim_discrete_time/flow_chart.png")

## ----include=TRUE, fig.align="center", fig.cap=c("A small DAG with time-varying age"), echo=FALSE, out.width=600----
knitr::include_graphics("./images_v_sim_discrete_time/simple_dag.png")

## -----------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(simDAG)

dag <- empty_dag() +
  node("age", type="rnorm", mean=30, sd=5) +
  node("sex", type="rbernoulli", p=0.5)

## -----------------------------------------------------------------------------
node_advance_age <- function(data) {
  return(data$age + 1)
}

## -----------------------------------------------------------------------------
prob_death <- function(data) {
  score <- -10 + 0.15 * data$age + 0.25 * data$sex
  prob <- 1/(1 + exp(-score))
  return(prob)
}

## -----------------------------------------------------------------------------
dag <- dag +
  node_td("age", type="advance_age", parents="age") +
  node_td("death", type="time_to_event", parents=c("age", "sex"),
          prob_fun=prob_death, event_duration=Inf, save_past_events=TRUE,
          check_inputs=FALSE)

## -----------------------------------------------------------------------------
node_td("death", type="time_to_event",
        formula= ~ -10 + 0.15*age + 0.25*sex,
        event_duration=Inf, save_past_events=TRUE,
        check_inputs=FALSE)

## ----fig.width=7, fig.height=5------------------------------------------------
plot(dag)

## -----------------------------------------------------------------------------
set.seed(43)
sim_dat <- sim_discrete_time(n_sim=10, dag=dag, max_t=50, check_inputs=FALSE)

## -----------------------------------------------------------------------------
head(sim_dat$data)

## ----fig.width=7, fig.height=5------------------------------------------------
plot(sim_dat)

## -----------------------------------------------------------------------------
set.seed(44)
sim_dat <- sim_discrete_time(n_sim=10, dag=dag, max_t=1000000,
                             break_if=all(data$death_event==TRUE),
                             check_inputs=FALSE)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("age", type="rnorm", mean=30, sd=5) +
  node("sex", type="rbernoulli", p=0.5)

## -----------------------------------------------------------------------------
prob_cve <- function(data) {
  score <- -15 + 0.15 * data$age + 0.25 * data$sex
  prob <- 1/(1 + exp(-score))
  return(prob)
}

## -----------------------------------------------------------------------------
dag <- dag +
  node_td("age", type="advance_age", parents=c("age")) +
  node_td("cve", type="time_to_event", parents=c("age", "sex"),
          prob_fun=prob_cve, event_duration=1, save_past_events=TRUE)

## -----------------------------------------------------------------------------
sim_dat <- sim_discrete_time(n_sim=10, dag=dag, max_t=50)
head(sim_dat$data)

## ----warning=FALSE------------------------------------------------------------
d_start_stop <- sim2data(sim_dat, to="start_stop")
head(d_start_stop)

## ----warning=FALSE------------------------------------------------------------
d_long <- sim2data(sim_dat, to="long")
head(d_long)

