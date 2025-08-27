## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center"
)

## -----------------------------------------------------------------------------
library(simDAG)

set.seed(1234)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("A", type="rgamma", shape=0.1, rate=2) +
  node("B", type="rbeta", shape1=2, shape2=0.3)

## -----------------------------------------------------------------------------
custom_root <- function(n, min=0, max=1, mean=0, sd=1) {
  out <- runif(n, min=min, max=max) + rnorm(n, mean=mean, sd=sd)
  return(out)
}

# the function may be supplied as a string
dag <- empty_dag() +
  node("A", type="custom_root", min=0, max=10, mean=5, sd=2)

# equivalently, the function can also be supplied directly
# This is the recommended way!
dag <- empty_dag() +
  node("A", type=custom_root, min=0, max=10, mean=5, sd=2)

data <- sim_from_dag(dag=dag, n_sim=100)
head(data)

## -----------------------------------------------------------------------------
node_gaussian_trunc <- function(data, parents, betas, intercept, error,
                                left, right) {
  out <- node_gaussian(data=data, parents=parents, betas=betas,
                       intercept=intercept, error=error)
  out <- ifelse(out <= left, left,
                ifelse(out >= right, right, out))
  return(out)
}

## -----------------------------------------------------------------------------
parents_sum <- function(data, parents, betas=NULL) {
  out <- rowSums(data[, parents, with=FALSE])
  return(out)
}

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("custom_1", type="gaussian_trunc", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=-2, error=2, left=10, right=25) +
  node("custom_2", type=parents_sum, parents=c("age", "custom_1"))

data <- sim_from_dag(dag=dag, n_sim=100)
head(data)

## -----------------------------------------------------------------------------
node_custom_root_td <- function(data, n, mean=0, sd=1) {
  return(rnorm(n=n, mean=mean, sd=sd))
}

## -----------------------------------------------------------------------------
n_sim <- 100

dag <- empty_dag() +
  node_td(name="Something", type=node_custom_root_td, n=n_sim, mean=10, sd=5)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td(name="Something", type=rnorm, mean=10, sd=5)

## -----------------------------------------------------------------------------
node_custom_child <- function(data, parents) {
  out <- numeric(nrow(data))
  out[data$other_event] <- rnorm(n=sum(data$other_event), mean=10, sd=3)
  out[!data$other_event] <- rnorm(n=sum(!data$other_event), mean=5, sd=10)
  return(out)
}

dag <- empty_dag() +
  node_td("other", type="time_to_event", prob_fun=0.1) +
  node_td("whatever", type="custom_child", parents="other_event")

## -----------------------------------------------------------------------------
node_square_sim_time <- function(data, sim_time, n_sim) {
  return(rep(sim_time^2, n=n_sim))
}

dag <- empty_dag() +
  node_td("unclear", type=node_square_sim_time, n_sim=100)

## -----------------------------------------------------------------------------
node_prev_state <- function(data, past_states, sim_time) {
  if (sim_time < 3) {
    return(rnorm(n=nrow(data)))
  } else {
    return(past_states[[sim_time-2]]$A + rnorm(n=nrow(data)))
  }
}

dag <- empty_dag() +
  node_td("A", type=node_prev_state, parents="A")

## -----------------------------------------------------------------------------
sim <- sim_discrete_time(dag, n_sim=100, max_t=10, save_states="all")

