## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(simDAG)
library(data.table)
library(survival)

set.seed(23414)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("Age", type="rnorm", mean=55, sd=5) +
  node("Sex", type="rbernoulli", p=0.5) +
  node("Treatment", type="rbernoulli", p=0.5) +
  node("Outcome", type="binomial",
       formula= ~ -12 + Age*0.2 + Sex*1.1 + Treatment*-0.5)

data <- sim_from_dag(dag, n_sim=1000)
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("Age", type="rnorm", mean=55, sd=5) +
  node("Sex", type="rbernoulli", p=0.5, output="numeric") +
  node("Treatment", type="rcategorical", probs=c(0.33333, 0.33333, 0.33333),
       labels=c("Placebo", "Med1", "Med2"), output="factor") +
  node("Outcome", type="binomial",
       formula= ~ -12 + Age*0.2 + Sex*1.1 + TreatmentMed1*-0.5 + 
         TreatmentMed2*-1)

data <- sim_from_dag(dag, n_sim=1000)
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("Age", type="rnorm", mean=55, sd=5) +
  node("Sex", type="rbernoulli", p=0.5, output="numeric") +
  node("Treatment", type="rbernoulli", p=0.5) +
  node_td("Outcome", type="gaussian",
          formula= ~ -12 + Age*0.2 + Sex*1.1 + Treatment*-0.5, error=1)

sim <- sim_discrete_time(dag, n_sim=1000, max_t=5, save_states="all")
data <- sim2data(sim, to="long")
head(data)

## -----------------------------------------------------------------------------
# function to calculate the probability of taking the pill at t,
# given the current treatment status of the person
prob_treat <- function(data) {
  fifelse(!data$Treatment_event, 0, 0.95)
}

dag <- empty_dag() +
  node("Treatment_event", type="rbernoulli", p=0.5) +
  node_td("Treatment", type="time_to_event", parents=c("Treatment_event"),
          prob_fun=prob_treat, event_count=TRUE, event_duration=1) +
  node_td("Outcome", type="gaussian", formula= ~ -2 + 
            Treatment_event_count*-0.3, error=2)
sim <- sim_discrete_time(dag, n_sim=1000, max_t=5, save_states="all")
data <- sim2data(sim, to="long")
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("Clinic", type="rcategorical", probs=rep(0.02, 50)) +
  node("Treatment", type="identity", formula= ~ Clinic >= 25) +
  node("Outcome", type="poisson", formula= ~ -1 + Treatment*4 + (1|Clinic),
       var_corr=0.5)
data <- sim_from_dag(dag, n_sim=1000)
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("cat", type="rcategorical", probs=c(0.4, 0.2, 0.2),
       labels=LETTERS[1:3]) +
  node("cont", type="rbeta", shape1=0.2, shape2=1.2) +
  node("treatment", type="binomial",
       formula= ~ -0.2 + catB*0.3 + catC*1 + cont*0.2) +
  node("outcome", type="gaussian",
       formula= ~ 3 + catB*1.1 + catC*0.2 + cont*-0.1,
       error=1)
data <- sim_from_dag(dag, n_sim=100)
head(data)

## -----------------------------------------------------------------------------
## function that generates the probability of treatment at t 
## for all individuals, given the current state of the simulation
prob_treat <- function(data, base_p, rr_treat, rr_outcome) {
  base_p * rr_treat^(data$treatment_event) * rr_outcome^(data$outcome_event)
}

## function that generates the probability of the outcome at t 
## for all individuals, given the current state of the simulation
prob_outcome <- function(data, base_p, rr_treat, rr_outcome) {
  base_p * rr_treat^(data$treatment_event) * rr_outcome^(data$outcome_event)
}

dag <- empty_dag() +
  node_td("treatment", type="time_to_event", prob_fun=prob_treat,
          parents=c("outcome_event"), event_duration=1,
          base_p=0.05, rr_treat=2, rr_outcome=0.5) +
  node_td("outcome", type="time_to_event", prob_fun=prob_outcome,
          parents=c("treatment_event"),
          event_duration=1, immunity_duration=40,
          base_p=0.01, rr_treat=0.3, rr_outcome=1.2)

sim <- sim_discrete_time(dag, n_sim=100, max_t=500)
data <- sim2data(sim, to="start_stop", overlap=TRUE)
head(data)

## -----------------------------------------------------------------------------
## function that generates the probability of the outcome at t 
## for all individuals, given the current state of the simulation
prob_Y <- function(data, base_p, rr_treat) {
  base_p * rr_treat^(data$A_event)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20) +
  node_td("Y", type="time_to_event", prob_fun=prob_Y,
          parents=c("A_event"),
          event_duration=Inf, base_p=0.01, rr_treat=0.5)

sim <- sim_discrete_time(dag, n_sim=500, max_t=500)
data <- sim2data(sim, to="start_stop", overlap=TRUE, target_event="Y",
                 keep_only_first=TRUE)
head(data)

## -----------------------------------------------------------------------------
mod <- coxph(Surv(start, stop, Y) ~ A, data=data)
summary(mod)

## -----------------------------------------------------------------------------
## function that generates the probability of the outcome at t 
## for all individuals, given the current state of the simulation
prob_Y <- function(data, intercept, beta_treat) {
  h <- intercept + beta_treat*data$A_event
  return(1 - exp(-(h)))
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20) +
  node_td("Y", type="time_to_event", prob_fun=prob_Y,
          parents=c("A_event"),
          event_duration=Inf, intercept=0.001, beta_treat=0.05)

sim <- sim_discrete_time(dag, n_sim=500, max_t=500)
data <- sim2data(sim, to="start_stop", overlap=TRUE, target_event="Y",
                 keep_only_first=TRUE)
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("school", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("female", type="rbernoulli", p=0.5) +
  node("age", type="rnorm", mean=12, sd=3) +
  node("score", type="gaussian",
       formula= ~ -2 + female*3 + age*0.1 + (1|school),
       var_corr=0.5, error=1)
data <- sim_from_dag(dag, n_sim=10)
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("strata", type="rbernoulli", p=0.5) +
  node(c("var1", "var2"), type="rnorm", mean=0, sd=1) +
  node("Y", type="mixture", parents=c("strata", "var1", "var2"),
       distr=list(
         "strata==0", node(".", type="gaussian",
                           formula= ~ -2 + var1*2 + var2*-0.5, error=1),
         "strata==1", node(".", type="gaussian",
                           formula= ~ 5 + var1*-1 + var2*2.3, error=1.5)
       ))
data <- sim_from_dag(dag, n_sim=10)
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm") +
  node("Y", type="mixture", parents=c("A", "B", "C"),
       distr=list(
         "TRUE", node(".", type="gaussian", formula= ~ -2 + A*0.1 + B*1 + C*-2,
                      error=1),
         "Y > 3", node(".", type="rnorm", mean=10000, sd=500)
       ))
data <- sim_from_dag(dag, n_sim=10000)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("A_real", type="rnorm", mean=10, sd=3) +
  node("A_missing", type="rbernoulli", p=0.5) +
  node("A_observed", type="identity",
       formula= ~ fifelse(A_missing, NA, A_real))

data <- sim_from_dag(dag, n_sim=10)
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("A_real", type="rnorm", mean=0, sd=1) +
  node("B_real", type="rbernoulli", p=0.5) +
  node("A_missing", type="rbernoulli", p=0.1) +
  node("B_missing", type="binomial", formula= ~ -5 + A_real*0.1) +
  node("A_observed", type="identity",
       formula= ~ fifelse(A_missing, NA, A_real)) +
  node("B_observed", type="identity",
       formula= ~ fifelse(B_missing, NA, B_real))

data <- sim_from_dag(dag, n_sim=10)
head(data)

## -----------------------------------------------------------------------------
probs <- list(`TRUE`=0.9, `FALSE`=0.01)

dag <- empty_dag() +
  node("Disease_real", type="rbernoulli", p=0.5) +
  node("Disease_observed", type="conditional_prob", parents="Disease_real",
       probs=probs)

data <- sim_from_dag(dag, n_sim=10)
head(data)

## -----------------------------------------------------------------------------
# first TRUE / FALSE refers to Sex = TRUE / FALSE
# second TRUE / FALSE refers to Disease = TRUE / FALSE
probs <- list(TRUE.TRUE=0.9, TRUE.FALSE=0.01,
              FALSE.TRUE=0.8, FALSE.FALSE=0.05)

dag <- empty_dag() +
  node("Sex", type="rbernoulli", p=0.5) +
  node("Disease_real", type="rbernoulli", p=0.5) +
  node("Disease_observed", type="conditional_prob",
       parents=c("Sex", "Disease_real"), probs=probs)

data <- sim_from_dag(dag, n_sim=1000)
head(data)

