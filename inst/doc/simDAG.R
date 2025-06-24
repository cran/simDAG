## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center",
  fig.width=5.5
)

## ----include=TRUE, fig.align="center", fig.cap=c("An example DAG with three nodes."), echo=FALSE, out.width=300----
knitr::include_graphics("./images_v_joss/example_dag.png")

## -----------------------------------------------------------------------------
set.seed(43)
n <- 100

A <- stats::rnorm(n)
B <- stats::rnorm(n)
C <- -2 + A*0.3 + B*-2 + stats::rnorm(n)

## ----eval=FALSE---------------------------------------------------------------
# node(name, type, parents=NULL, formula=NULL, ...)
# 
# node_td(name, type, parents=NULL, formula=NULL, ...)

## ----message=FALSE, warning=FALSE---------------------------------------------
library("simDAG")
dag <- empty_dag() +
  node(c("A", "B"), type="rnorm", mean=0, sd=1) +
  node("C", type="gaussian", formula=~-2 + A*0.3 + B*-2,
       error=1)

## ----message=FALSE, warning=FALSE---------------------------------------------
library("igraph")
library("ggplot2")
library("ggforce")
plot(dag, layout="as_tree", node_size=0.15, node_fill="grey",
     node_text_fontface="italic")

## -----------------------------------------------------------------------------
summary(dag)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node(c("X1", "X3"), type="rbernoulli", p=0.5,
       output="numeric") +
  node(c("X4", "X6"), type="rnorm", mean=0, sd=1) +
  node("X2", type="gaussian", formula=~0.3 + X3*0.1,
       error=1) +
  node("X5", type="gaussian", formula=~0.3 + X6*0.1,
       error=1) +
  node("Z", type="binomial", formula=~-1.2 + I(X2^2)*log(3)
        + X3*log(1.5) + X5*log(1.5) + X6*log(2),
       output="numeric") +
  node("T", type="cox", formula=~X1*log(1.8) + X2*log(1.8) +
        X4*log(1.8) + I(X5^2)*log(2.3) + Z*-1,
       surv_dist="weibull", lambda=2, gamma=2.4,
       cens_dist="rweibull",
       cens_args=list(shape=1, scale=2))

## -----------------------------------------------------------------------------
plot(dag, node_size=0.3, node_fill="grey",
     node_text_fontface="italic")

## -----------------------------------------------------------------------------
summary(dag)

## ----message=FALSE, warning=FALSE---------------------------------------------
library("data.table")
dat <- sim_from_dag(dag, n_sim=500)
head(round(dat, 3))

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("U", type="rbernoulli", p=0.5, output="numeric") +
  node("L0", type="gaussian", formula=~0.1 + 0.6*U,
       error=1) +
  node("A0", type="binomial", formula=~-0.4 + 0.6*L0,
       output="numeric") +
  node("Y1", type="binomial", formula=~-3.5 + -0.9*U,
       output="numeric") +
  node("L1", type="gaussian", formula=~0.5 + 0.02*L0 +
        0.5*U + 1.5*A0, error=1) +
  node("A1", type="binomial", formula=~-0.4 + 0.02*L0 +
        0.58*L1, output="numeric") +
  node("Y2", type="binomial", formula=~-2.5 + 0.9*U,
       output="numeric")

## -----------------------------------------------------------------------------
plot(dag, node_size=0.2, node_fill="grey",
     node_text_fontface="italic", layout="in_circle")

## -----------------------------------------------------------------------------
summary(dag)

## -----------------------------------------------------------------------------
dat <- sim_from_dag(dag, n_sim=1000)
head(dat)

## ----echo=FALSE---------------------------------------------------------------
set.seed(129)

dag <- empty_dag() +
  node("Y_0", type="rnorm") +
  node("Y_1", type="binomial", formula=~ 1 + Y_0*2) +
  node("Y_2", type="binomial", formula=~ 1 + Y_1*3)

plot(dag, node_size=0.3, node_fill="grey",
     node_text_fontface="italic")

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("Y", type="time_to_event", prob_fun=0.01,
          event_duration=Inf)

## -----------------------------------------------------------------------------
sim <- sim_discrete_time(dag, n_sim=1000, max_t=80)

## -----------------------------------------------------------------------------
head(sim$data)

## ----include=TRUE, fig.align="center", fig.cap=c("A simple graph showing $P_A(t)$ and $P_Y(t)$ for a fictional individual who got vaccinated once at $t = 100$, with $P_{A0} = 0.01$, $P_{Y0} = 0.005$, $d_{risk} = 20$ and $RR_A = 3.24$."), echo=FALSE, out.width=600----
knitr::include_graphics("./images_v_joss/example_probs.png")

## -----------------------------------------------------------------------------
prob_myoc <- function(data, P_0, RR_A) {
  fifelse(data$A_event, P_0*RR_A, P_0)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20, immunity_duration=150) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event"), P_0=0.005, RR_A=3.24)

## -----------------------------------------------------------------------------
sim <- sim_discrete_time(dag, n_sim=10000, max_t=365*2)

## -----------------------------------------------------------------------------
dat <- sim2data(sim, to="start_stop", target_event="Y",
                keep_only_first=TRUE, overlap=TRUE)
head(dat)

## ----message=FALSE, warning=FALSE---------------------------------------------
library("survival")
mod <- coxph(Surv(start, stop, Y) ~ A, data=dat)
summary(mod)

## -----------------------------------------------------------------------------
prob_vacc <- function(data, P_0) {
  fifelse(data$Y_event, 0, P_0)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=prob_vacc,
          event_duration=20, immunity_duration=150, P_0=0.01) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event"), P_0=0.005, RR_A=3.24,
          event_duration=80)

## ----eval=FALSE---------------------------------------------------------------
# # NOTE: This part of the code is not run here, because it would take too long
# # on CRAN and would introduce another dependency on the "microbenchmark"
# # package. Results may also vary depending on the hardware this is run on.
# 
# library(microbenchmark)
# 
# set.seed(1234)
# 
# prob_myoc <- function(data, P_0, RR_A) {
#   fifelse(data$A_event, P_0*RR_A, P_0)
# }
# 
# run_example <- function(n, max_t) {
#   dag <- empty_dag() +
#     node_td("A", type="time_to_event", prob_fun=0.01,
#             event_duration=20, immunity_duration=150) +
#     node_td("Y", type="time_to_event", prob_fun=prob_myoc,
#             parents=c("A_event"), P_0=0.005, RR_A=3.24)
# 
#   sim <- sim_discrete_time(dag, n_sim=n, max_t=max_t)
# 
#   dat <- sim2data(sim, to="start_stop", target_event="Y",
#                   keep_only_first=TRUE, overlap=TRUE)
# }
# 
# n <- c(10, 100, 1000, 10000, 100000)
# max_t <- c(10, 100, 1000, 10000)
# 
# params <- data.frame(max_t=rep(max_t, each=length(n)),
#                      n=rep(n), time=NA)
# 
# for (i in seq_len(nrow(params))) {
#   n_i <- params$n[i]
#   max_t_i <- params$max_t[i]
# 
#   bench <- microbenchmark(run_example(n=n_i, max_t=max_t_i),
#                           times=1)
#   params$time[i] <- mean(bench$time / 1000000000)
# }
# 
# params <- within(params, {
#   max_t <- factor(max_t);
# })
# 
# ggplot(params, aes(x=n, y=time, color=max_t)) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   labs(x="n_sim", y="Runtime in Seconds", color="max_t") +
#   scale_x_continuous(labels=scales::label_comma(),
#                      transform="log10") +
#   scale_y_log10()

## ----include=TRUE, fig.align="center", echo=FALSE, out.width=600--------------
knitr::include_graphics("./images_v_joss/runtime.png")

## -----------------------------------------------------------------------------
prob_myoc <- function(data, P_0, RR_A, sim_time) {
  P_0 <- P_0 + 0.001*sim_time
  fifelse(data$A_event, P_0*RR_A, P_0)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20, immunity_duration=150) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event"), P_0=0.005, RR_A=3.24)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200)
data <- sim2data(sim, to="start_stop")
head(data)

## -----------------------------------------------------------------------------
prob_myoc <- function(data, P_0, RR_A, sim_time) {
  RR_A <- RR_A + 0.01*sim_time
  fifelse(data$A_event, P_0*RR_A, P_0)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20, immunity_duration=150) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event"), P_0=0.005, RR_A=3.24)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200)
data <- sim2data(sim, to="start_stop")
head(data)

## -----------------------------------------------------------------------------
prob_myoc <- function(data, P_0, RR_A) {
  RR_A <- RR_A - 0.1*data$A_time_since_last
  fifelse(data$A_event, P_0*RR_A, P_0)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20, immunity_duration=150,
          time_since_last=TRUE) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event", "A_time_since_last"),
          P_0=0.005, RR_A=3.24)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200)
data <- sim2data(sim, to="start_stop")
head(data)

## -----------------------------------------------------------------------------
prob_myoc <- function(data, P_0, RR_A, RR_C) {
  P_0 * RR_A^(data$A_event) * RR_C^(data$C_event)
}

prob_covid <- function(data, P_0, d_immune) {
  fifelse(data$A_time_since_last < d_immune, 0, P_0, na=P_0)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20, immunity_duration=150,
          time_since_last=TRUE) +
  node_td("C", type="time_to_event", prob_fun=prob_covid,
          parents=c("A_time_since_last"), event_duration=14,
          P_0=0.01, d_immune=120) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event", "A_time_since_last", "C_event"),
          P_0=0.005, RR_A=3.24, RR_C=2.5)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200)
data <- sim2data(sim, to="start_stop")
head(data)

## -----------------------------------------------------------------------------
prob_myoc <- function(data, P_0, RR_A) {
  fifelse(data$A_event, P_0*RR_A, P_0)
}

prob_vacc <- function(data, P_0) {
  fifelse(data$Sex, P_0*2, P_0)
}

dag <- empty_dag() +
  node("Sex", type="rbernoulli", p=0.4) +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20, immunity_duration=150) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event"), P_0=0.005, RR_A=3.24)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200)
data <- sim2data(sim, to="start_stop")
head(data)

## -----------------------------------------------------------------------------
prob_myoc <- function(data, P_0, RR_A) {
  fifelse(data$A_event > 0, P_0*RR_A, P_0)
}

prob_vacc <- function(data) {
  n <- nrow(data)
  p_mat <- matrix(c(rep(0.99, n), rep(0.0005, n),
                    rep(0.0005, n)),
                  byrow=FALSE, ncol=3)
  return(p_mat)
}

dag <- empty_dag() +
  node_td("A", type="competing_events", prob_fun=prob_vacc,
          event_duration=c(20, 20), immunity_duration=150) +
  node_td("Y", type="time_to_event", prob_fun=prob_myoc,
          parents=c("A_event"), P_0=0.005, RR_A=3.24)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200,
                         save_states="all")
data <- sim2data(sim, to="start_stop")
head(data)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("calories", type="rnorm", mean=2500, sd=150) +
  node_td("calories", type="gaussian",
          formula= ~ 1 + calories*1.1, error=1)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200,
                         save_states="all")
data <- sim2data(sim, to="long")
head(data)

## -----------------------------------------------------------------------------
prob_bachelors <- function(data) {
  fifelse(data$highschool_event, 0.01, 0)
}

prob_masters <- function(data) {
  fifelse(data$bachelors_event, 0.01, 0)
}

dag <- empty_dag() +
  node_td("highschool", type="time_to_event", prob_fun=0.01,
          event_duration=Inf) +
  node_td("bachelors", type="time_to_event", prob_fun=prob_bachelors,
          event_duration=Inf, parents="highschool_event") +
  node_td("masters", type="time_to_event", prob_fun=prob_masters,
          event_duration=Inf, parents="bachelors_event")

sim <- sim_discrete_time(dag, n_sim=100, max_t=200)
data <- sim2data(sim, to="start_stop")
head(data)

