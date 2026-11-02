## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(simDAG)
library(data.table)

dag <- empty_dag() +
  node("X", type="rnorm", mean=0, sd=1) +
  node("A", type="binomial", formula= ~ -2 + X*1.1) +
  node("Y", type="binomial", formula= ~ -1.2 + A*0.7 + X*0.4)

set.seed(1234)
data <- sim_from_dag(dag, n_sim=10000)
head(data)

## -----------------------------------------------------------------------------
mod_A <- glm(A ~ X, data=data, family="binomial")
summary(mod_A)

## -----------------------------------------------------------------------------
mod_Y <- glm(Y ~ A + X, data=data, family="binomial")
summary(mod_Y)

## -----------------------------------------------------------------------------
mean(data$A)

## -----------------------------------------------------------------------------
mean(data$Y)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  dag <- empty_dag() +
    node("X", type="rnorm", mean=0, sd=1.5) +
    node("A", type="binomial", formula= ~ -2 + X*1.1) +
    node("Y", type="binomial", formula= ~ param(intercept_Y) + A*0.7 + X*0.4)
)

## -----------------------------------------------------------------------------
targets <- list(
  prevalence_Y = list(target = 0.3, eval_fun = function(data) {mean(data$Y)})
)

## -----------------------------------------------------------------------------
params <- list(
  intercept_Y = list(start = 0, range=c(-5, 5))
)

## -----------------------------------------------------------------------------
cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 10000,
  seed = 42
)

summary(cdag)

## -----------------------------------------------------------------------------
data <- sim_from_dag(cdag$dag, n_sim=100)
head(data)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  dag <- empty_dag() +
    node("X", type="rnorm", mean=0, sd=1.5) +
    node("A", type="binomial", formula= ~ param(intercept_A) + X*1.1) +
    node("Y", type="binomial", formula= ~ -1 + A*0.7 + X*0.4)
)

targets <- list(
  prevalence_A = list(target = 0.2, eval_fun = function(data) {mean(data$A)})
)

params <- list(
  intercept_A = list(start = 0, range=c(-5, 5))
)

cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 10000,
  seed = 42
)

summary(cdag)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  dag <- empty_dag() +
    node("X", type="rnorm", mean=0, sd=1.5) +
    node("A", type="binomial", formula= ~ -2.023 + X*1.1) +
    node("Y", type="binomial", formula= ~ param(intercept_Y) + A*0.7 + X*0.4)
)

targets <- list(
  prevalence_Y = list(target = 0.4, eval_fun = function(data) {mean(data$Y)})
)

params <- list(
  intercept_Y = list(start = 0, range=c(-5, 5))
)

cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 10000,
  seed = 42
)

summary(cdag)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  dag <- empty_dag() +
    node("X", type="rnorm", mean=0, sd=1.5) +
    node("A", type="binomial", formula= ~ param(intercept_A) + X*1.1) +
    node("Y", type="binomial", formula= ~ param(intercept_Y) + A*0.7 + X*0.4)
)

targets <- list(
  prevalence_A = list(target = 0.2, eval_fun = function(data) {mean(data$A)}),
  prevalence_Y = list(target = 0.4, eval_fun = function(data) {mean(data$Y)})
)

params <- list(
  intercept_A = list(start = 0, range=c(-5, 5)),
  intercept_Y = list(start = 0, range=c(-5, 5))
)

cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 10000,
  seed = 42
)

summary(cdag)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  empty_dag() +
    node("X", type="rnorm", mean=0, sd=1) +
    node("A", type="binomial", formula= ~ 0.005 + X*log(3)) +
    node("Y", type="binomial", formula= ~ -1.2 + A*0.7 + X*param(betaYX))
)

## -----------------------------------------------------------------------------
# a function that evaluates the bias in a given dataset
eval_bias <- function(data) {
  fit_unadjusted <- glm(Y ~ A, family="binomial", data=data)
  fit_adjusted <- glm(Y ~ A + X, family="binomial", data=data)
  bias <- coef(fit_unadjusted)[["ATRUE"]] - coef(fit_adjusted)[["ATRUE"]]
  return(bias)
}

targets <- list(
  bias = list(target = 0.6, eval_fun = eval_bias)
)

## -----------------------------------------------------------------------------
params <- list(start = 0, range=c(-5, 5))

## -----------------------------------------------------------------------------
cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 10000,
  seed = 11
)

summary(cdag)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  empty_dag() +
    node("X", type="rnorm", mean=0, sd=1) +
    node("A", type="binomial", formula= ~ 0.005 + X*param(betaAX)) +
    node("Y", type="binomial", formula= ~ -1.2 + A*0.7 + X*param(betaYX))
)

targets <- list(
  bias = list(target = 0.6, eval_fun = eval_bias)
)

params <- list(
  betaAX = list(start = 0, range=c(-5, 5)),
  betaYX = list(start = 0, range=c(-5, 5))
)

cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 10000,
  seed = 43
)

summary(cdag)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  empty_dag() +
    node(c("X1", "X2"), type="rnorm", mean=0, sd=1) +
    node("A", type="binomial", formula= ~ 0.005 + X1*log(3) + X2*log(1.3)) +
    node("Y", type="cox", formula= ~ A*log(0.6) + X1*log(2) + X2*log(3),
         surv_dist="exponential", lambda=0.01,
         cens_dist="rexp", cens_args=list(rate=param(cens)))
)

eval_cens_prop <- function(data) {
  1 - mean(data$Y_status)
}

targets <- list(
  cens_prop = list(target = 0.65, eval_fun = eval_cens_prop)
)
params <- list(start = 0.1, range=c(0.0000001, 5))

cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 10000,
  seed = 12
)

summary(cdag)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  empty_dag() +
    node("U", type="rnorm") +
    node_td("Y", type="next_time", formula= ~ param(intercept) + U*log(1.5),
            event_duration=1)
)

## -----------------------------------------------------------------------------
targets <- list(
  mean_events = list(target = 5, eval_fun = function(x) {
    n_id <- x[, .(n_events = sum(Y)), by=.id]
    mean(n_id$n_events)
  })
)

params <- list(start = 1, range = c(-3, 2))

fit <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 1000,
  n_sim_eval = 10000,
  seed = 132,
  max_t = 50,
  censor_at_max_t = TRUE,
  target_event = "Y"
)

summary(fit)

## -----------------------------------------------------------------------------
ldag <- lazy_dag(
  empty_dag() +
    node("X", type="rnorm", mean=0, sd=1) +
    node("A", type="binomial", formula= ~ 0.005 + X*log(3)) +
    node("Y", type="binomial", formula= ~ -1.2 + A*param(beta) + X*log(2))
)

eval_marginal_OR <- function(data) {
  
  # fit conditional model
  model <- glm(Y ~ A + X, data=data, family="binomial")
  
  # use g-computation to get marginal probabilities
  data$A <- 1
  p1 <- mean(predict(model, newdata=data, type="response"))
  
  data$A <- 0
  p0 <- mean(predict(model, newdata=data, type="response"))
  
  # calculate marginal OR
  odds_treated <- p1 / (1 - p1)
  odds_control <- p0 / (1 - p0)

  marginal_or  <- odds_treated / odds_control
  return(marginal_or)
}

targets <- list(
  marginal_OR = list(target = 0.6, eval_fun = eval_marginal_OR)
)

params <- list(
  beta = list(start = -0.5, range=c(-1, 0))
)

cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 1000,
  n_sim_eval = 3000,
  seed = 47
)

summary(cdag)

## -----------------------------------------------------------------------------
eval_marginal_OR2 <- function(dag) {
  
  # use the do() function to set A to 0 or 1 respectively
  dag0 <- do(dag, "A", 0)
  dag1 <- do(dag, "A", 1)
  
  # simulate data under both counterfactual regimes
  data0 <- sim_from_dag(dag0, n_sim=100000)
  data1 <- sim_from_dag(dag1, n_sim=100000)
  
  # take the observed proportions as marginal probabilities
  p0 <- mean(data0$Y)
  p1 <- mean(data1$Y)
  
  # calculate marginal OR
  odds_treated <- p1 / (1 - p1)
  odds_control <- p0 / (1 - p0)

  marginal_or  <- odds_treated / odds_control
  return(marginal_or)
}

# same as before, but with different function and use_dag = TRUE
targets <- list(
  marginal_OR = list(target = 0.6, eval_fun = eval_marginal_OR2,
                     use_dag = TRUE)
)

cdag <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 1000,
  n_sim_eval = 3000,
  seed = 43
)

summary(cdag)

