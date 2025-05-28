## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>"
)

## -----------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(simDAG)

dag <- empty_dag() +
  node_td("vaccination", type="time_to_event", prob_fun=0.001,
          event_duration=21, immunity_duration=Inf) +
  node_td("covid", type="time_to_event", prob_fun=0.001, event_duration=30,
          immunity_duration=80) +
  node_td("sickness", type="time_to_event", prob_fun=0.0001,
          event_duration=2, immunity_duration=2)

## -----------------------------------------------------------------------------
prob_sickness <- function(data, rr_covid, rr_vacc, base_p) {

  # multiply base probability by relevant RRs
  p <- base_p * rr_vacc^(data$vaccination_event) * rr_covid^(data$covid_event)

  return(p)
}

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("vaccination", type="time_to_event", prob_fun=0.001,
          event_duration=21, immunity_duration=Inf) +
  node_td("covid", type="time_to_event", prob_fun=0.001, event_duration=30,
          immunity_duration=80) +
  node_td("sickness", type="time_to_event", prob_fun=prob_sickness,
          parents=c("vaccination_event", "covid_event"),
          base_p=0.0001, rr_covid=3.5, rr_vacc=3.24,
          event_duration=2, immunity_duration=2)

## -----------------------------------------------------------------------------
prob_covid <- function(data, base_p, vacc_duration) {
  
  p <- fifelse(data$vaccination_time_since_last < vacc_duration,
               0, base_p, na=base_p)
  return(p)
}

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("vaccination", type="time_to_event", prob_fun=0.001,
          event_duration=21, immunity_duration=Inf,
          time_since_last=TRUE) +
  node_td("covid", type="time_to_event", prob_fun=prob_covid,
          parents=c("vaccination_time_since_last"),
          base_p=0.001, vacc_duration=80, event_duration=30,
          immunity_duration=80) +
  node_td("sickness", type="time_to_event", prob_fun=prob_sickness,
          parents=c("vaccination_event", "covid_event"),
          base_p=0.0001, rr_covid=3.5, rr_vacc=3.24,
          event_duration=2, immunity_duration=2)

## -----------------------------------------------------------------------------
prob_vaccination <- function(data, base_p) {
  
  p <- fifelse(data$covid_event, 0, base_p)
  
  return(p)
}

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("vaccination", type="time_to_event",
          prob_fun=prob_vaccination,
          parents=c("covid_event"), base_p=0.001,
          event_duration=21, immunity_duration=Inf,
          time_since_last=TRUE) +
  node_td("covid", type="time_to_event", prob_fun=prob_covid,
          parents=c("vaccination_time_since_last"),
          base_p=0.001, vacc_duration=80, event_duration=30,
          immunity_duration=80) +
  node_td("sickness", type="time_to_event", prob_fun=prob_sickness,
          parents=c("vaccination_event", "covid_event"),
          base_p=0.0001, rr_covid=3.5, rr_vacc=3.24,
          event_duration=2, immunity_duration=2)

## ----fig.width=7, fig.height=5------------------------------------------------
plot(dag, mark_td_nodes=FALSE)

## -----------------------------------------------------------------------------
set.seed(42)
sim <- sim_discrete_time(dag, n_sim=1000, max_t=800)
summary(sim)

## ----fig.width=7, fig.height=5------------------------------------------------
plot(sim, box_text_size=4)

## -----------------------------------------------------------------------------
sim2data(sim, to="start_stop")

