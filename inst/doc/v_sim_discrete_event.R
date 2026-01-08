## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>"
)

## ----include=TRUE, fig.align="center", fig.cap=c("A generalized flow-chart of the discrete-event simulation approach"), echo=FALSE, out.width=500----
knitr::include_graphics("./images_v_sim_discrete_event/flow_chart.png")

## -----------------------------------------------------------------------------
library(simDAG)
library(data.table)

set.seed(1234)

dag_dts <- empty_dag() +
  node_td("death", type="time_to_event", prob_fun=0.01, event_duration=Inf)

simDTS <- sim_discrete_time(dag_dts, n_sim=10, max_t=10000000,
                            break_if=all(data$death_event==TRUE))
head(simDTS$data)

## -----------------------------------------------------------------------------
dag_des <- empty_dag() +
  node_td("death", type="next_time", prob_fun=0.01, event_duration=Inf)

simDES <- sim_discrete_event(dag_des, n_sim=10, target_event="death",
                             keep_only_first=TRUE)
head(simDES)

## -----------------------------------------------------------------------------
prob_death <- function(data) {
  0.001 * 0.8^(data$treatment)
}

dag <- empty_dag() +
  node_td("treatment", type="next_time", prob_fun=0.01,
          event_duration=100) +
  node_td("death", type="next_time", prob_fun=prob_death,
          event_duration=Inf)

sim <- sim_discrete_event(dag, n_sim=10, remove_if=death==TRUE,
                          target_event="death", keep_only_first=TRUE)

## -----------------------------------------------------------------------------
head(sim, 9)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("treatment", type="next_time", prob_fun=0.01,
          event_duration=100) +
  node_td("death", type="next_time",
          formula= ~ log(0.001) + log(0.8)*treatment, link="log",
          event_duration=Inf)

sim <- sim_discrete_event(dag, n_sim=10, remove_if=death==TRUE,
                          target_event="death", keep_only_first=TRUE)

## -----------------------------------------------------------------------------
prob_death <- function(data) {
  
  base_p <- fifelse(data$.time > 300, 0.005, 0.001)
  
  base_p * 0.8^(data$treatment)
}

dag <- empty_dag() +
  node_td("treatment", type="next_time", prob_fun=0.01,
          event_duration=100) +
  node_td("death", type="next_time", prob_fun=prob_death,
          event_duration=Inf)

sim <- sim_discrete_event(dag, n_sim=10, remove_if=death==TRUE,
                          target_event="death", redraw_at_t=300,
                          keep_only_first=TRUE)
head(sim)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("treatment", type="next_time", prob_fun=0.01,
          event_duration=100) +
  node_td("death", type="next_time",
          formula= ~ log(0.8)*treatment, model="cox",
          surv_dist="weibull", gamma=1.5, lambda=0.0001,
          event_duration=Inf)

sim <- sim_discrete_event(dag, n_sim=10, remove_if=death==TRUE,
                          target_event="death", keep_only_first=TRUE)
head(sim)

## -----------------------------------------------------------------------------
integer_rtexp <- function(n, rate, l) {
  ceiling(rtexp(n=n, rate=rate, l=l))
}

dag <- empty_dag() +
  node_td("treatment", type="next_time", prob_fun=0.01,
          event_duration=100, distr_fun=integer_rtexp) +
  node_td("death", type="next_time",
          formula= ~ log(0.001) + log(0.8)*treatment, link="log",
          event_duration=Inf, distr_fun=integer_rtexp)

sim <- sim_discrete_event(dag, n_sim=1000, remove_if=death==TRUE,
                          target_event="death", allow_ties=TRUE,
                          keep_only_first=TRUE)

